## code to prepare `calanus_par` dataset goes here

calanus_par <- c(
    r = 0.107782157552587,
    b = 0.0697821575525873,
    K = 10,
    v = 0.04130139511238,
    g = 0,
    a = 0.03989704,
    m = 0.337262,
    L = 3048.66850321396,
    f = 1
)

usethis::use_data(calanus_par, overwrite = TRUE)
