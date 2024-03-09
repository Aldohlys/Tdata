#### test getBSCallPrice ####
test_that("getBSCallPrice works with a first set of values", {
  expect_equal(round(getBSCallPrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0),5),
               1.19455)
})

test_that("getBSCallPrice works with a second set of values", {
  expect_equal(round(getBSCallPrice(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0),7),
               0.1265669)
})


#### test getBSPutPrice ####
test_that("getBSPutPrice works with a first set of values", {
  expect_equal(round(getBSPutPrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0),5),
               1.13977)
})

test_that("getBSPutPrice works with a second set of values", {
  expect_equal(round(getBSPutPrice(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0),7),
               0.6142415)
})

#### test getBSStraddlePrice ####
test_that("getBSStraddlePrice works with a first set of values", {
  expect_equal(round(getBSStraddlePrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0),5),
               1.19455+1.13977)
})

test_that("getBSStraddlePrice works with a first set of values", {
  expect_equal(round(getBSStraddlePrice(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0),5),
               round(0.1265669+0.6142415,5))
})

### test getBSPrice #####
test_that("getBSPrice works correctly",{
  expect_equal({
    round(getBSPrice(type="Put",S=100,K=100,DTE=5,sig=0.3,r=0.05035,mul=100),4)
  },136.6021)
})



### test getBSComboPrice #####
test_that("getBSComboPrice works with a first set of values", {
  expect_equal({
      df=cbind(data.frame(pos=c(2,-2),type=c("Put","Put"),strike=c(100,95),DTE=c(5,10)),sig=0.3,r=0.05035,mul=100)
      round(getBSComboPrice(df,S=100),4)
    }, 100.7629)
})

test_that("getBSComboPrice works with a second set of values", {
  expect_equal({
      df=cbind(data.frame(pos=c(1,-2),type=c("Call","Put"),strike=c(100,95),DTE=c(5,10)),sig=0.4,r=0.05035,mul=100)
      round(getBSComboPrice(df,S=90),3)
    }, -1119.979)
})

test_that("getBSComboPrice works with a third set of values - here PGCD equals 2, stocks are included", {
  expect_equal({
      df=cbind(data.frame(pos=c(200,2),type=c("Stock","Put"),strike=c(NA,127),DTE=c(NA,18),
                    sig=c(0,0.37),mul=c(1,100)),r=0.05)
      round(getBSComboPrice(df,S=130),2)
    }, 13275.52)
})


test_that("getBSComboPrice works with a fourth set of values,
              a long Diagonal spread with sig included but different values
          - this time with 2 different sig in the dataframe
          and PGCD equals 3", {
            expect_equal({
              df=cbind(data.frame(pos=c(3,-3),type=c("Put","Put"),strike=c(280,285),DTE=c(25,150),
                                  sig=c(0.45,0.25)),mul=100,r=as.numeric(config::get("interest_rate")))
              round(getBSComboPrice(df,S=285),2)
            },-457.23)
})

test_that("getBSComboPrice works with a vectorized set of S underlying values - using previous tests",{
  expect_equal({
    df=cbind(data.frame(pos=c(2,-2),type=c("Put","Put"),strike=c(100,95),DTE=c(5,10)),sig=0.3,mul=100)
    S=93:102
    r=0.05035
    round(getBSComboPrice(data=df,S=S,r=r),2)
  },c(401.38, 365.00, 322.98, 276.92, 229.09, 182.14, 138.67, 100.76 , 69.67,  45.71))
})

### test getImpliedVolOpt #####
test_that("getImpliedVolOpt runs correctly with one set of parameters",{
  expect_equal(
    round(getImpliedVolOpt(type="Call",S=100,K=105,DTE=5,price=0.06303),2),
    0.25
  )
})

test_that("getImpliedVolOpt runs correctly with a second set of parameters",{
  expect_equal(
    round(getImpliedVolOpt(type="Put",S=90,K=100,DTE=10.88,price=9.93758),2),
    0.35
  )
})

test_that("getImpliedIROpt runs correctly with a set of parameters",{
  expect_equal(
    round(getImpliedIROpt("Call",S=100,K=100,DTE=5,sig=0.25,div=0,price=1.19455),2),
    0.04
  )
})
