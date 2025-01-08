SELECT
  m.target_cohort_id,
  m.ordinal_id
  m.time_label,
  m.line_item_label,
  m.value_id,
  COUNT(SUBJECT_ID) AS value
FROM (
    SELECT d.*
    FROM #pat_ts_tab
    WHERE d.ordinal = {ordinalId}
) m
GROUP BY target_cohort_id, ordinal_id, time_label, line_item_label, value_id




