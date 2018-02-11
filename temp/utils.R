toy_cstDs <- list(type = "cstamp",
                  names = list(A = "res1A_focus_hand_mdg_20171117"),
                  data = res1A_focus_hand_cstamp[[1]]$data[1:5, ]
                 )
str(toy_cstDs)

save(toy_cstDs, file = "./temp/toys.RData")

load("./temp/toys.RData")
