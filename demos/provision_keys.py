#!/usr/bin/env python3
"""
provision_keys.py

Provision a batch of OpenRouter API keys for students in a class, each with
a per-key daily spending limit enforced natively by OpenRouter.

Usage:
    export OPENROUTER_MGMT_KEY="sk-or-v1-...(your Management/Provisioning key)..."
    python provision_keys.py --count 95 --limit 0.30 --prefix class-2026 --out keys.csv

    # Preview without calling the API:
    python provision_keys.py --count 95 --limit 0.30 --dry-run

Requires: requests  (pip install requests --break-system-packages)

Security notes:
    - The Management key is read from the OPENROUTER_MGMT_KEY environment
      variable (or a .env file in the same directory). It is never hardcoded
      and never written to the output CSV.
    - The output CSV contains the full student-facing inference keys in
      plaintext. Treat it as sensitive: don't commit it, store it somewhere
      access-controlled, and delete/rotate keys after the course if needed.
"""

import argparse
import csv
import json
import os
import sys
import time
from pathlib import Path

try:
    import requests
except ImportError:
    print("This script requires the 'requests' package.\n"
          "Install it with: pip install requests --break-system-packages",
          file=sys.stderr)
    sys.exit(1)

API_BASE = "https://openrouter.ai/api/v1/keys"
RECOVERY_LOG = "provisioning-recovery.log"


def load_mgmt_key() -> str:
    """Read the Management API key from env var or a local .env file."""
    key = os.environ.get("OPENROUTER_MGMT_KEY")
    if key:
        return key.strip()

    env_path = Path(__file__).parent / ".env"
    if env_path.exists():
        for line in env_path.read_text().splitlines():
            line = line.strip()
            if not line or line.startswith("#") or "=" not in line:
                continue
            k, _, v = line.partition("=")
            if k.strip() == "OPENROUTER_MGMT_KEY":
                return v.strip().strip('"').strip("'")

    print(
        "ERROR: OPENROUTER_MGMT_KEY not found.\n"
        "Set it as an environment variable, e.g.:\n"
        "  export OPENROUTER_MGMT_KEY='sk-or-v1-...'\n"
        "or create a .env file next to this script containing:\n"
        "  OPENROUTER_MGMT_KEY=sk-or-v1-...\n"
        "\n"
        "Get a Management (Provisioning) key from OpenRouter Settings ->\n"
        "Provisioning API Keys. This is different from a normal inference key.",
        file=sys.stderr,
    )
    sys.exit(1)


def create_key(mgmt_key: str, name: str, limit: float, limit_reset: str,
                expires_at: str | None, timeout: float = 20.0) -> dict:
    """Create a single OpenRouter API key via the Management API."""
    payload = {
        "name": name,
        "limit": limit,
        "limit_reset": limit_reset,
    }
    if expires_at:
        payload["expires_at"] = expires_at

    resp = requests.post(
        API_BASE,
        headers={
            "Authorization": f"Bearer {mgmt_key}",
            "Content-Type": "application/json",
        },
        json=payload,
        timeout=timeout,
    )
    resp.raise_for_status()
    return resp.json()


def append_recovery_log(entry: dict) -> None:
    with open(RECOVERY_LOG, "a") as f:
        f.write(json.dumps(entry) + "\n")


def already_provisioned(out_path: Path) -> set[str]:
    """Return the set of 'name' values already present in an existing CSV,
    so re-running the script can fill gaps instead of duplicating keys."""
    if not out_path.exists():
        return set()
    names = set()
    with open(out_path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            if row.get("name"):
                names.add(row["name"])
    return names


def main():
    parser = argparse.ArgumentParser(
        description="Provision per-student OpenRouter API keys with daily spending limits."
    )
    parser.add_argument("--count", type=int, default=95,
                         help="Number of keys to create (default: 95)")
    parser.add_argument("--limit", type=float, default=0.30,
                         help="Daily spending limit in USD per key (default: 0.30)")
    parser.add_argument("--limit-reset", default="daily", choices=["daily", "weekly", "monthly"],
                         help="Limit reset interval (default: daily)")
    parser.add_argument("--expires-at", default=None,
                         help="Optional ISO 8601 expiry date/time for all keys, "
                              "e.g. 2026-12-15T00:00:00Z. Omit for no expiry.")
    parser.add_argument("--prefix", default="class-2026",
                         help="Name prefix for keys, e.g. 'class-2026' -> class-2026-001 (default: class-2026)")
    parser.add_argument("--start-index", type=int, default=1,
                         help="Starting index for key names (default: 1)")
    parser.add_argument("--out", default="keys.csv",
                         help="Output CSV path (default: keys.csv)")
    parser.add_argument("--dry-run", action="store_true",
                         help="Print what would be done without calling the API")
    parser.add_argument("--delay", type=float, default=0.25,
                         help="Seconds to sleep between API calls, to be gentle on rate limits (default: 0.25)")
    args = parser.parse_args()

    width = max(3, len(str(args.start_index + args.count - 1)))
    names = [f"{args.prefix}-{i:0{width}d}"
             for i in range(args.start_index, args.start_index + args.count)]

    out_path = Path(args.out)
    existing_names = already_provisioned(out_path)
    todo = [n for n in names if n not in existing_names]

    if existing_names:
        print(f"Found existing {out_path} with {len(existing_names)} keys already provisioned; "
              f"skipping those and creating {len(todo)} remaining.")

    if args.dry_run:
        print(f"[DRY RUN] Would create {len(todo)} keys:")
        print(f"  limit=${args.limit:.2f} USD, limit_reset={args.limit_reset}, "
              f"expires_at={args.expires_at or 'none'}")
        for n in todo[:5]:
            print(f"  - {n}")
        if len(todo) > 5:
            print(f"  ... and {len(todo) - 5} more")
        print(f"Would write results to: {out_path}")
        return

    mgmt_key = load_mgmt_key()

    write_header = not out_path.exists()
    created, failed = 0, 0

    with open(out_path, "a", newline="") as f:
        writer = csv.writer(f)
        if write_header:
            writer.writerow(["index", "name", "key", "hash", "daily_limit_usd", "expires_at"])

        for i, name in zip(range(args.start_index, args.start_index + args.count), names):
            if name not in todo:
                continue
            try:
                result = create_key(
                    mgmt_key, name, args.limit, args.limit_reset, args.expires_at
                )
                data = result.get("data", result)
                secret_key = result.get("key") or data.get("key")
                key_hash = data.get("hash", "")

                writer.writerow([i, name, secret_key, key_hash, args.limit, args.expires_at or ""])
                f.flush()
                created += 1
                print(f"[{i}] created {name}")
            except Exception as e:
                failed += 1
                print(f"[{i}] FAILED to create {name}: {e}", file=sys.stderr)
                append_recovery_log({"index": i, "name": name, "error": str(e)})

            time.sleep(args.delay)

    print()
    print(f"Done. Created: {created}, Failed: {failed}")
    if failed:
        print(f"See {RECOVERY_LOG} for failure details. Re-run this script with the same "
              f"--out {out_path} to fill in the missing keys (already-created ones are skipped).")
    print(f"Results written to: {out_path}")
    print("\nReminder: keys.csv contains plaintext secrets. Store it securely and "
          "don't commit it to a public repo.")


if __name__ == "__main__":
    main()
