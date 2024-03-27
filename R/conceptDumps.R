charlsonConcepts <- function() {
  conceptSet <- list(
    Capr::cs(Capr::descendants(4329847), name = "Acute Myocardial Infarction"),
    Capr::cs(Capr::descendants(316139), name = "Congestive Heart Failure"),
    Capr::cs(Capr::descendants(321052), name = "Peripheral Vascular Disease"),
    Capr::cs(Capr::descendants(381591, 434056), name = "Cerebrovascular Disease"),
    Capr::cs(Capr::descendants(4182210), name = "Dementia"),
    Capr::cs(Capr::descendants(4063381), name = "Chronic Pulmonary Disease"),
    Capr::cs(Capr::descendants(257628, 134442, 80800, 80809, 256197, 255348), name = "Rheumatologic Disease"),
    Capr::cs(Capr::descendants(4247120), name = "Peptic ulcer disease"),
    Capr::cs(Capr::descendants(4064161, 4212540), name = "Mild liver disease"),
    Capr::cs(Capr::descendants(201820), name = "Diabetes (mild to moderate)"),
    Capr::cs(Capr::descendants(443767, 442793), name = "Diabetes with chronic complications"),
    Capr::cs(Capr::descendants(192606, 374022), name = "Hemoplegia or paralegia"),
    Capr::cs(Capr::descendants(4030518), name = "Renal disease"),
    Capr::cs(Capr::descendants(443392), name = "Any malignancy"),
    Capr::cs(Capr::descendants(4245975, 4029488, 192680, 24966), name = "Moderate to severe liver disease"),
    Capr::cs(Capr::descendants(432851), name = "Metastatic solid tumor"),
    Capr::cs(Capr::descendants(439727), name = "AIDS")
  )
  return(conceptSet)
}
