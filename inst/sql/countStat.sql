/* Count Stat transformation */
INSERT INTO @dataTable (cohort_id, subject_id, category_id, time_id, value_id, value)
SELECT
    d.cohort_id,
    d.subject_id,
    d.time_id,
    d.value_id,
    COUNT(DISTINCT d.record_date) AS value
FROM {{csTempTableName}} d
GROUP BY d.cohort_id, d.subject_id, d.time_id, d.value_id
;
