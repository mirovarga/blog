module Parser = struct
  type metadata = { title : string; created : string; draft : bool }
  type post = { metadata : metadata; location : string }

  let chars_to_string ch = List.to_seq ch |> String.of_seq |> String.trim

  let skip_whitespace =
    let is_whitespace = function
      | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
      | _ -> false
    in
    Angstrom.skip_while is_whitespace

  let title_p =
    let open Angstrom in
    skip_whitespace *> char '#' *> many_till any_char end_of_line
    >>| chars_to_string

  let created_iso_p =
    let open Angstrom in
    let* month' = skip_whitespace *> char '*' *> take 3 in
    let* day = char ' ' *> (take 1 <* char ',' <|> (take 2 <* char ',')) in
    let* year = char ' ' *> take 4 <* char '*' in
    let month m =
      List.assoc m
        [
          ("Jan", 1);
          ("Feb", 2);
          ("Mar", 3);
          ("Apr", 4);
          ("May", 5);
          ("Jun", 6);
          ("Jul", 7);
          ("Aug", 8);
          ("Sep", 9);
          ("Oct", 10);
          ("Nov", 11);
          ("Dec", 12);
        ]
    in
    return
      (Printf.sprintf "%s%s%02d%s%02d" year "-" (month month') "-"
         (int_of_string day))

  let draft_p =
    let open Angstrom in
    skip_whitespace *> string "*Draft*" *> return true

  let metadata_p =
    let open Angstrom in
    let* title = title_p in
    let* created = created_iso_p in
    let* draft = option false draft_p in
    return { title; created; draft }

  let metadata_of_md md =
    match Angstrom.parse_string ~consume:Prefix metadata_p md with
    | Ok _ as p -> p
    | Error _ -> Error "Cannot parse md"

  let post_of_md_file path =
    let ch = In_channel.open_text path in
    let md = In_channel.input_all ch in
    In_channel.close_noerr ch;

    let metadata = metadata_of_md md in
    match metadata with
    | Ok m -> Ok { metadata = m; location = path }
    | Error _ as e -> e

  let blog_of_md_dir dir =
    let mds =
      Sys.readdir dir |> Array.to_list
      |> List.map (FilePath.concat dir)
      |> List.filter (fun f ->
             Filename.extension f = ".md" && (not @@ Sys.is_directory f))
    in
    List.map post_of_md_file mds
    |> List.filter Result.is_ok |> List.map Result.get_ok
end

module SSG = struct
  let index_html ps =
    let html_template =
      Printf.sprintf
        {|<!doctype html>
  
        <html>
        <head>
          <meta charset="utf-8">
          <meta name="viewport" content="width=device-width, initial-scale=1">
  
          <title>%s - Miro Varga</title>
          
          <link href="/main.min.css" rel="stylesheet">
        </head>
  
        <body class="container mx-auto px-10 prose lg:text-lg">
          <header class="mt-5 mb-10">
            <a href="/" class="no-underline text-gray-400">MIRO VARGA</a>
          </header>
          
          <ul>%s</ul>

          <footer class="mt-20 mb-10 text-sm text-gray-400">
            Hi, I'm Miro Varga, a software developer based in Prague, Czechia.
            Check my work 
            <a href="https://github.com/mirovarga" class="text-inherit">GitHub</a>,
            see my CV 
            <a href="https://www.linkedin.com/in/miro-varga-670002187" class="text-inherit">LinkedIn</a>,
            or send me an email to
            <a href="mailto:hello@mirovarga.com" class="text-inherit">hello@mirovarga.com</a>.
          </footer>
        </body>
        </html>
        |}
    in
    let body =
      List.map (fun (p : Parser.post) ->
          let url =
            let base =
              Filename.basename @@ p.location |> Filename.remove_extension
            in
            base ^ ".html"
          in
          Printf.sprintf {|<li><a href="%s">%s</a></li>|} url p.metadata.title)
      @@ List.fast_sort
           (fun (p1 : Parser.post) p2 ->
             compare p2.metadata.created p1.metadata.created)
           ps
      |> List.fold_left ( ^ ) ""
    in
    html_template "Home" @@ Printf.sprintf "%s" body

  let post_html (p : Parser.post) =
    let html_template =
      Printf.sprintf
        {|<!doctype html>
      
      <html>
      <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
  
        <title>%s - Miro Varga</title>
        
        <link href="/main.min.css" rel="stylesheet">
      </head>
      
      <body class="container mx-auto px-10 prose lg:text-lg">
        <header class="mt-5 mb-10">
          <a href="/" class="no-underline text-gray-400">MIRO VARGA</a>
        </header>
        
        <article>%s</article>
        
        <footer class="mt-20 mb-10 text-sm text-gray-400">
          Hi, I'm Miro Varga, a software developer based in Prague, Czechia.
          Check my work 
          <a href="https://github.com/mirovarga" class="text-inherit">GitHub</a>,
          see my CV 
          <a href="https://www.linkedin.com/in/miro-varga-670002187" class="text-inherit">LinkedIn</a>,
          or send me an email to
          <a href="mailto:hello@mirovarga.com" class="text-inherit">hello@mirovarga.com</a>.
        </footer>
      </body>
      </html>
      |}
    in
    let ch = In_channel.open_text p.location in
    let html = In_channel.input_all ch |> Omd.of_string |> Omd.to_html in
    In_channel.close ch;
    (p, html_template p.metadata.title html)

  let post_file_to_html_file src_path dest_path =
    let post = Parser.post_of_md_file src_path in
    match post with
    | Ok p ->
        let html = post_html p in
        let ch = Out_channel.open_text dest_path in
        Out_channel.output_string ch @@ snd html;
        Out_channel.close_noerr ch;
        Printf.printf "Generated '%s'\n" dest_path
    | Error e -> Printf.printf "Error generating '%s': %s\n" dest_path e

  let posts_dir_to_html_dir src_dir dest_dir =
    let generate_index ps =
      let index_path = FilePath.concat dest_dir "index.html" in
      let ch = Out_channel.open_text index_path in
      Out_channel.output_string ch @@ index_html ps;
      Out_channel.close ch;
      Printf.printf "Generated '%s'\n" index_path
    in

    let generate_posts =
      List.iter (fun (p : Parser.post) ->
          post_file_to_html_file p.location
          @@ Filename.concat dest_dir
               ((Filename.basename @@ p.location |> Filename.remove_extension)
               ^ ".html"))
    in

    let posts =
      Parser.blog_of_md_dir src_dir
      |> List.filter (fun (p : Parser.post) -> not p.metadata.draft)
    in

    FileUtil.rm ~recurse:true [ dest_dir ];
    FileUtil.mkdir ~parent:true ~mode:(`Octal 0o700) dest_dir;
    generate_index posts;
    generate_posts posts
end
