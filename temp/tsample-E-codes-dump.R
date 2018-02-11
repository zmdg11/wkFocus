err_inx <- which(agree_df$d == "E")
err_inx
err_t <- agree_df$t[err_inx]
err_t
tmp <- t(agree_list)
tmp <- as.data.frame(tmp)
tmp$t <- as.character(tmp$t)
err_t <- as.character(err_t)
err_list <- filter(tmp, t %in% err_t)
c1_err <- filter(coder1_df, t %in% err_t)
c1_err
c2_err <- filter(coder2_df, t %in% err_t)
c2_err


