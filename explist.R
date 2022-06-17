
alist <- list(
  "A" = list(a = 1,
             b = 2),
  "B" = list(c = 3,
             d = 4)
)

vec <- c("A_a", "A_b", "B_c", "B_d")
newlist <- list()

vec <- list("A" = c("a","b"),"B" = c("c","d"))

c("a","b") |> set_names() |> map(~{1})

names(vec) |> 
  set_names() |> 
map(~{
  parent_name <- .
  child_name <- vec[[.]]
  child_name |> 
    set_names() |> 
    map(~{alist[[parent_name]][[.]]})
})


imap(c("a","b"), ~{print(.x);print(.y)})


xx <- xtable::xtable(tibble(a = "hi there and \n this is new"))
print(xx,sanitize.text.function = identity)
print(xx,sanitize.text.function = NULL)
