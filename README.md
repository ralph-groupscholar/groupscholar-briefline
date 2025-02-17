# Group Scholar Briefline

Briefline is a lightweight CLI for turning scholar session notes into structured briefs and storing them in Postgres. It standardizes summaries, actions, risks, and follow-ups so the team can scan recent work quickly.

## Features
- Parse Markdown notes with standard headings into structured data
- Store briefs in Postgres with a dedicated schema
- List recent briefs and export a single brief as JSON
- Dry-run mode for verifying parsing without writing to the database

## Requirements
- Emacs (for running the CLI)
- PostgreSQL (remote production database)

## Setup
1. Copy `.env.example` to `.env` and fill in the Postgres credentials.
2. Export the environment variables in your shell.
3. Run database setup:

```sh
scripts/setup-db.sh
```

## Usage
```sh
bin/briefline ingest path/to/note.md
bin/briefline ingest path/to/note.md --dry-run
bin/briefline list --limit 10
bin/briefline export --id <uuid>
```

### Expected Markdown Format
```md
Scholar: Ava Reynolds
Session Date: 2026-02-04

## Summary
Ava is preparing final materials for the STEM summer program.

## Actions
- Review statement for clarity
- Confirm program start date

## Risks
Potential conflict with family commitments.

## Follow-ups
- Send revised feedback by Feb 10
```

## Tests
```sh
emacs --batch -l test/briefline-core-test.el -f ert-run-tests-batch-and-exit
```

## Technology
- Emacs Lisp
- PostgreSQL
