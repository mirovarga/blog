let () =
  try
    let src_dir, dest_dir = (Sys.argv.(1), Sys.argv.(2)) in
    Blog.SSG.posts_dir_to_html_dir src_dir dest_dir
  with Invalid_argument _ -> print_endline "Usage: <src_dir> <dest_dir>"
