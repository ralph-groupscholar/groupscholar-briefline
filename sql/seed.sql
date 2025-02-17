insert into briefline.briefs
  (scholar_name, session_date, summary, risks, actions, followups, source_path)
values
  (
    'Ava Reynolds',
    '2026-02-04',
    'Ava is preparing final materials for the STEM summer program and requested feedback on her personal statement tone.',
    'Potential conflict with family commitments during the program start week.',
    '["Review statement for clarity", "Confirm program start date with Ava"]'::jsonb,
    '["Send revised feedback by Feb 10", "Check in on travel logistics"]'::jsonb,
    '/notes/ava-reynolds-2026-02-04.md'
  ),
  (
    'Jordan Lee',
    '2026-02-05',
    'Jordan completed the scholarship interview prep and feels confident about the narrative arc.',
    'Interview slot may be rescheduled due to campus closure.',
    '["Share interview checklist", "Draft backup scheduling email"]'::jsonb,
    '["Follow up after interview", "Capture interview reflections"]'::jsonb,
    '/notes/jordan-lee-2026-02-05.md'
  );
