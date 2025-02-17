create extension if not exists pgcrypto;

create schema if not exists briefline;

create table if not exists briefline.briefs (
  id uuid primary key default gen_random_uuid(),
  scholar_name text not null,
  session_date date not null,
  summary text not null,
  risks text not null,
  actions jsonb not null default '[]'::jsonb,
  followups jsonb not null default '[]'::jsonb,
  source_path text not null,
  created_at timestamptz not null default now()
);

create index if not exists briefs_created_at_idx
  on briefline.briefs (created_at desc);
