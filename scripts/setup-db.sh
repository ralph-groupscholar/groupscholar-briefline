#!/bin/sh
set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

if [ -z "$PGHOST" ] || [ -z "$PGPORT" ] || [ -z "$PGUSER" ] || [ -z "$PGPASSWORD" ] || [ -z "$PGDATABASE" ]; then
  echo "Missing required database environment variables." >&2
  exit 1
fi

psql -v ON_ERROR_STOP=1 -f "$PROJECT_DIR/sql/schema.sql"
psql -v ON_ERROR_STOP=1 -f "$PROJECT_DIR/sql/seed.sql"
