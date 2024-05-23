test_that("vector of strategies is returned by DB", {
  expect_identical(getStrategies(),
               c("BOT",    "BPT" ,   "Dan" ,   "Erreur" ,"LTO"  ,  "OFI" ,
                "Perso" , "WHEEL" ))
})
