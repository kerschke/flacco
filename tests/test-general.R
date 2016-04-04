library(testthat)

# test general stuff:
test_check("flacco", filter = "^base")

print("TRAVIS:")
print(Sys.getenv("TRAVIS"))
print("R_EXPENSIVE_TEST_OK:")
print(Sys.getenv("R_EXPENSIVE_TEST_OK"))

if (identical(Sys.getenv("TRAVIS"), "true") || identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true")) {
  test_check("flacco", filter = "^plot")
}