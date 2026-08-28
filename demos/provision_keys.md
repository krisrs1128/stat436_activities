# Provisioning OpenRouter API Keys for a Class

A one-time script to create per-student OpenRouter API keys with a daily
spending cap, for distribution via Canvas or Piazza private messages.

## 1. Get a Management (Provisioning) key

1. Go to OpenRouter → Settings → **Provisioning API Keys**.
2. Create a new Provisioning key. This is different from a normal inference
   key — it can create/modify/delete other keys on your account.
3. Save it somewhere safe. You'll use it once, not distribute it.

## 2. Set up the script

```bash
pip install requests --break-system-packages   # only dependency

export OPENROUTER_MGMT_KEY="sk-or-v1-...your provisioning key..."
```

(Alternatively, create a `.env` file next to `provision_keys.py` with a line
`OPENROUTER_MGMT_KEY=sk-or-v1-...`.)

## 3. Dry run first

```bash
python provision_keys.py --count 95 --limit 0.30 --dry-run
```

This prints what would be created without calling the API — a good sanity
check on the count, naming, and limit before spending any quota.

## 4. Provision for real

```bash
python provision_keys.py --count 95 --limit 0.30 --prefix class-2026 --out keys.csv
```

Defaults: `--count 95`, `--limit 0.30` (i.e. $0.30/day per key), `--limit-reset daily`,
no expiry. Each flag can be overridden; run `python provision_keys.py --help`
for the full list.

This creates 95 keys named `class-2026-001` … `class-2026-095`, each capped
at $0.30/day (resetting at midnight UTC), with no expiration date, and
writes them to `keys.csv` with columns:

```
index, name, key, hash, daily_limit_usd, expires_at
```

The `key` column is the actual secret to hand to a student. The `hash`
column is OpenRouter's identifier for that key, useful if you later want to
look up usage or disable a specific key via the Management API.

## 5. If something fails partway through

The script writes each row to `keys.csv` as it goes (not all at once at the
end), and skips any `name` already present in that file. So if a run is
interrupted or some requests fail, just re-run the same command — it will
only create the missing keys. Failures are also logged to
`provisioning-recovery.log` with the error message.

## 6. Distribute the keys

Open `keys.csv` and paste each student's key into an individual private
Canvas message or Piazza note — one key per student, e.g.:

> Your personal OpenRouter API key for this course is:
> `sk-or-v1-...`
> It's capped at $0.30/day and resets at midnight UTC. Please don't share it.

Keep `keys.csv` as your master record, e.g. for re-sending a key a student
lost. **Treat this file as sensitive** — it contains plaintext secrets.
Don't commit it to a public repo; store it somewhere access-controlled and
delete it (or rotate the keys) after the course ends if you're done with it.

## 7. Checking usage or adjusting a key later (optional)

The Management API also supports reading usage and updating an individual
key by its hash, if you ever need to check on or adjust a specific
student's key:

```bash
# Check usage
curl -s https://openrouter.ai/api/v1/keys/<hash> \
  -H "Authorization: Bearer $OPENROUTER_MGMT_KEY"

# Disable a key
curl -s -X PATCH https://openrouter.ai/api/v1/keys/<hash> \
  -H "Authorization: Bearer $OPENROUTER_MGMT_KEY" \
  -H "Content-Type: application/json" \
  -d '{"disabled": true}'
```

## Out of scope (by design)

This is intentionally the minimal "Option A" setup: no Discord bot, no
database mapping keys to individual student identities, and no self-serve
key lookup page. If you want students to self-serve their own key or check
their own usage later, that's a separate, slightly bigger "Option B"
(a small password-protected page) — not needed for a one-time provisioning
step.
