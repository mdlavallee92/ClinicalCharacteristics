SELECT
        d.target_cohort_id,
        d.subject_id,
        d.time_label,
        d.domain_table,
        'presence' AS aggregation_type,
        d.raw_occurrence_id as value_id,
        COUNT(DISTINCT d.event_date) AS value
INTO @patient_level_data
FROM @concept_set_occurrence_table d
GROUP BY d.target_cohort_id, d.subject_id, d.time_label, d.domain_table, d.raw_occurrence_id
;
