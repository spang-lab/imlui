# compare_dbs <- function(db1 = default_db(),
#                         db2 = configured_db(),
#                         file1 = "tbls1.md",
#                         file2 = "tbls2.md",
#                         ignore = c(
#                           "mapping_users_sessions"
#                         )) {
#   tbls1 <- db1$get_tables()
#   tbls2 <- db2$get_tables()
#   cat("Storing Tables from DB 1 in", file1, "\n")
#   write_dfs(dfs = tbls1, file = file1, ignore = ignore)
#   cat("Storing Tables from DB 2 in", file2, "\n")
#   write_dfs(dfs = tbls2, file = file2, ignore = ignore)
# }

# write_dfs <- function(dfs, ignore, file = stdout()) {
#   # df1 <- data.frame(matrix(rnorm(25), 5, 5))
#   # df2 <- data.frame(matrix(rnorm(18), 6, 3))
#   # df3 <- data.frame(matrix(rnorm(12), 3, 4))
#   # dfs <- list(Helloworld_DF = df1, Superman_DF = df2, XYZ_DF = df3)
#   # write_dfs(dfs)
#   md_secs <- rep("", length(dfs))
#   for (i in seq_along(dfs)) {
#     tbl_name <- names(dfs)[i]
#     if (tbl_name %in% ignore) {
#       next
#     }
#     df <- dfs[[i]]
#     df_out <- capture.output(write.table(df, file = stdout(), sep="\t"))
#     df_str <- paste(df_out, collapse = "\n")
#     md_secs[[i]] <- sprintf("# %s\n\n```csv\n%s\n```\n", tbl_name, df_str)
#   }
#   md <- paste(md_secs, collapse = "\n")
#   cat(md, file = file)
#   invisible(md)
# }
