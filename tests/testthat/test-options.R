#### test getBSCallPrice ####
test_that("getBSCallPrice works with a first set of values", {
  expect_equal(getBSCallPrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0),
               1.195)
})

test_that("getBjSCallPrice works with a first set of values", {
  expect_equal(getBjSCallPrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0),
               1.194)
})


test_that("getBSCallPrice works with a second set of values", {
  expect_equal(getBSCallPrice(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0),
               0.127)
})


#### test getBSPutPrice ####
test_that("getBSPutPrice works with a first set of values", {
  expect_equal(getBSPutPrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0),
               1.14)
})

test_that("getBSPutPrice works with a second set of values", {
  expect_equal(getBSPutPrice(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0),
               0.614)
})

#### test getBSStraddlePrice ####
test_that("getBSStraddlePrice works with a first set of values", {
  expect_equal(getBSStraddlePrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0),
               2.335)
})

test_that("getBSStraddlePrice works with a second set of values", {
  expect_equal(getBSStraddlePrice(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0),
               0.741)
})

### test getBSPrice #####
test_that("getBSPrice works correctly",{
  expect_equal({
    getBSPrice(type="Put",S=100,K=100,DTE=5,sig=0.3,r=0.05035,mul=100)
  },136.60)
})



### test getBSComboPrice #####
test_that("getBSComboPrice works with a first set of values", {
  expect_equal({
      df=cbind(data.frame(pos=c(2,-2),type=c("Put","Put"),strike=c(100,95),DTE=c(5,10)),sig=0.3,r=0.05035,mul=100)
      getBSComboPrice(df,S=100)
    }, 100.8)
})

test_that("getBSComboPrice works with a second set of values", {
  expect_equal({
      df=cbind(data.frame(pos=c(1,-2),type=c("Call","Put"),strike=c(100,95),DTE=c(5,10)),sig=0.4,r=0.05035,mul=100)
      getBSComboPrice(df,S=90)
    }, -1120)
})

test_that("getBSComboPrice works with a third set of values - here PGCD equals 2, stocks are included", {
  expect_equal({
      df=cbind(data.frame(pos=c(200,2),type=c("Stock","Put"),strike=c(NA,127),DTE=c(NA,18),
                    sig=c(0,0.37),mul=c(1,100)),r=0.05)
      getBSComboPrice(df,S=130)
    }, 13275.5)
})


test_that("getBSComboPrice works with a fourth set of values,
              a long Diagonal spread with sig included but different values
          - this time with 2 different sig in the dataframe
          and PGCD equals 3", {
            expect_equal({
              df=cbind(data.frame(pos=c(3,-3),type=c("Put","Put"),strike=c(280,285),DTE=c(25,150),
                                  sig=c(0.45,0.25)),mul=100,r=0.0531)
              getBSComboPrice(df,S=285)
            },-464.6)
})

test_that("getBSComboPrice works with a vectorized set of S underlying values - using previous tests",{
  expect_equal({
    df=cbind(data.frame(pos=c(2,-2),type=c("Put","Put"),strike=c(100,95),DTE=c(5,10)),sig=0.3,mul=100)
    S=93:102
    r=0.05035
    getBSComboPrice(data=df,S=S,r=r)
  },c(401.3, 365.0, 323, 277, 229.1, 182.1, 138.7, 100.8, 69.7,  45.7))
})

### test getImpliedVolOpt #####
test_that("getImpliedVolOpt runs correctly with one set of parameters",{
  expect_equal(
    getImpliedVolOpt(type="Call",S=100,K=105,DTE=5,price=0.06303),
    0.25
  )
})

test_that("getImpliedVolOpt runs correctly with a second set of parameters",{
  expect_equal(
    getImpliedVolOpt(type="Put",S=90,K=100,DTE=10.88,price=9.93758),
    0.347
  )
})

test_that("getImpliedIROpt runs correctly with a set of parameters",{
  expect_equal(
   getImpliedIROpt("Call",S=100,K=100,DTE=5,sig=0.25,div=0,price=1.19455),
    0.04
  )
})
