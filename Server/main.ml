open Printf

let command_array = [|"show";"get";"quit"|];;

let open_file path =
  let ic = open_in path in
  let arr = ref [||] in
  try
    while true; do
      let line = input_line ic in
      arr := Array.append !arr [|line|];
    done; close_in ic; !arr
  with End_of_file ->
    flush stdout;
    close_in ic;
    !arr;;

let get_param line =
  let c = ref 0 in
  let run = ref 1 in
  while (!c != ((String.length line) - 1) && !run == 1) do
    if (String.get line !c) == ';' then (run := 0; incr c) else incr c;
  done;
  String.sub line !c ((String.length line) - (!c));;

let show_dir outchan =
  let dir = (Sys.getcwd ()) in
  let children = Sys.readdir dir in
  output_string outchan ((string_of_int ((Array.length children) - 1)) ^"\n");
  flush outchan;
  for i = 0 to ((Array.length children) -1) do
    output_string outchan (children.(i)^"\n");
    flush outchan;
  done;;

let print_wd outchan =
  let wdir = (Sys.getcwd ()) in
  output_string outchan ("0"^"\n");
  flush outchan;
  output_string outchan (wdir^"\n");
  flush outchan;;

let rm_file outchan usr_cmd = 
  let file_name = get_param usr_cmd in
  Sys.remove file_name;
  output_string outchan ("0"^"\n");
  flush outchan;
  output_string outchan (file_name^" was removed from folder"^"\n");
  flush outchan;;

let change_wd outchan usr_cmd = 
  let path = get_param usr_cmd in
  Sys.chdir path;
  output_string outchan ("0"^"\n");
  flush outchan;
  output_string outchan ("Current Directory: "^(Sys.getcwd ())^"\n");
  flush outchan;;

let ftp_command inchan =
  fprintf stdout "Waiting for client's command...\n";
  flush stdout;
  let user_cmd = input_line inchan in 
  user_cmd;;

let get_user_cmd line =
  let c = ref 0 in
  let run = ref 1 in
  while (!c != (String.length line - 1) && !run == 1) do
    if (String.get line !c) == ';' then run := 0 else incr c
  done;
  int_of_string (String.sub line 0 !c);;

let close_connexion outchan sock = 
  close_out outchan;
  printf "Connexion closed\n";
  Unix.close sock;;

let send_file outchan usr_cmd = 
  let file = open_file (get_param usr_cmd) in
  output_string outchan ((string_of_int ((Array.length file) - 1)) ^"\n");
  flush outchan;
  for i = 0 to (Array.length file) -1 do
    output_string outchan (file.(i)^"\n");
    flush outchan;
  done;;

let establish_connexion =
  fprintf stdout "Waiting for client's connexion...\n";
  flush stdout;
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 8080));
  Unix.listen sock 5;
  let run = ref 1 in
  while true do
      let (s, _caller) = Unix.accept sock in
      match Unix.fork() with
        0 -> if Unix.fork() <> 0 then exit 0; 
          fprintf stdout "Connected!\n";
          flush stdout;
          let inchan = Unix.in_channel_of_descr s in
          let outchan = Unix.out_channel_of_descr s in
          while (!run == 1) do
            let usr_cmd = ftp_command inchan in
            (match (get_user_cmd (usr_cmd)) with
            |0 -> show_dir outchan
            |1 -> send_file outchan usr_cmd
            |2 -> close_connexion outchan sock; run := 0; exit 0
            |3 -> rm_file outchan usr_cmd
            |4 -> print_wd outchan
            |5 -> change_wd outchan usr_cmd
            |_ -> ());
          done;
        | id -> Unix.close s; ignore(Unix.waitpid [] id)
  done;;