SELECT
  t.cohort_definition_id AS target_cohort_id,
  t.subject_id,
  'Static at Index' AS time_label,
  'person' AS domain_table,
  'ConceptSetDemographic' AS aggregation_type,
  '{valueId}' AS  value_id,
  1 AS value
FROM @targetTable t
JOIN @cdmDatabaseSchema.person d
ON t.subject_id = d.person_id
-- WHERE d.conceptColumn IN (conceptIds) /* Removed temporarily*/
;
