SELECT
  m.target_cohort_id,
  m.ordinal_id,
  m.time_label,
  m.line_item_label,
  m.patient_line,
  COUNT(DISTINCT SUBJECT_ID) AS subject_count
INTO @categorical_table
FROM (
    SELECT d.*
    FROM #pat_ts_tab d
    WHERE d.statistic_type = 'presence'
) m
GROUP BY target_cohort_id, ordinal_id, time_label, line_item_label
;




