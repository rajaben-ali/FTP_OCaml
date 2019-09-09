open Printf;;

let str_to_word_array str sep =
    let str_size = ((String.length str) - 1) in
    let result = ref [||] in
    let keep = ref "" in
    let add_to_list list str_to_add =
        Array.append list [|str_to_add|] in
    let end_loop_cases keep count result = match !keep with
        | "" -> !result
        | _ -> keep := !keep ^ (String.make 1 str.[count]);
        result := (add_to_list !result !keep); keep := ""; !result
    in
    let rec stw_loop count progress =
        if (str_size = count) then (end_loop_cases keep count result)
        else
            (if (str.[count] == sep)
                then (stw_loop (count + 1) progress)
            else
                (if ((count + 1) = String.length str || str.[count + 1] == sep)
                    then (keep := !keep ^ (String.make 1 str.[count]);
                        result := (add_to_list !result !keep); keep := "";
                        (stw_loop (count + 1) (progress + 1)))
                else
                        (keep := !keep ^ (String.make 1 str.[count]);
                        (stw_loop (count + 1) progress))))
    in
    stw_loop 0 0
;;

let file = "file.txt";;

type commandes =
  | Show
  | Get of string
  | Quit
  | Help
  | Rm of string
  | Pwd
  | Cd of string
;;


let string_array_to_commandes (arr : string array) =
    let len = Array.length arr in
    if len = 0 then
        None
    else
        match arr.(0), len with
        | "show", 1 -> Some Show
        | "get" , 2  -> Some (Get arr.(1))
        | "quit", 1 -> Some Quit
        | "help", 1 -> Some Help
        | "rm", 2 -> Some (Rm arr.(1))
        | "pwd", 1 -> Some Pwd
        | "cd", 2 -> Some (Cd arr.(1))
        | _ -> None
;;

let show_help () =
    output_string stdout "Command list: show, get, show, help, rm and pwd\n\n\t\
    show : to show the server's folder\n\t\
    get <filename> : to get the content of the file given as parameter\n\t\
    quit : to quit the program and cut the connexion\n\t\
    rm <filename> : to remove a file from the current folder\n\t\
    pwd: to print the working directory\n";
    flush stdout;  
;;

let write_in_file file_name oc ic = 
    let fd = open_out_gen [Open_append ; Open_creat] 0o666 file_name in
    let r = input_line ic in 
    Printf.fprintf  fd "%s\n" r;
    flush oc
;;

let construct_cmd indx str_array_cmd =
    let sent_buff = ref "" in
    sent_buff := ((string_of_int indx) ^ ";");
    (if Array.length str_array_cmd == 2 then sent_buff := (!sent_buff ^ str_array_cmd.(1)));
    sent_buff := !sent_buff ^ "\n";
    !sent_buff
;;

let do_command indx str_array_cmd oc ic =
    let sent_buff = (construct_cmd indx str_array_cmd) in 
    output_string oc (sent_buff);
    flush oc;
    let loop_nb =  int_of_string (input_line ic) in
    for i = 0 to loop_nb do
        match indx with
        | 1 -> (write_in_file str_array_cmd.(1) oc ic)
        | _ -> (output_string stdout ((input_line ic) ^ "\n"); flush stdout)
    done
;;

let ask_for_retry () =
    printf "Unable to connect to the server !\nWould you like to retry ? [y/n]\n";
    flush stdout;
    let rep = input_line stdin in
    if (String.compare rep "y" == 0) then 1
    else 0
;;

let rec client_func () =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    printf "Welcome to Our File Transfert Protocol written in OCaml.\n
    Establishing connexion to the server ...\n\n";
    flush stdout;
    try
        Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string "192.168.1.1", 8080));
        let rec asking () =
            print_string "Enter your command : ";
            flush  stdout;
            let oc = (Unix.out_channel_of_descr sock) in
            let ic = (Unix.in_channel_of_descr sock) in
            let r = input_line stdin in
            let str_array_cmd = (str_to_word_array r ' ') in
                match string_array_to_commandes str_array_cmd with
                | None ->
                    Printf.printf "Command not found\n"; flush stdout; asking ()
                | Some cmd ->
                    (match cmd with
                    | Help    -> show_help (); asking ()
                    | Show    -> (do_command 0 str_array_cmd oc ic); asking ()
                    | Get str -> (do_command 1 str_array_cmd oc ic); asking ()
                    | Quit    -> output_string oc "2;\n"; flush oc; ()
                    | Rm str  -> (do_command 3 str_array_cmd oc ic); asking ()
                    | Pwd     -> (do_command 4 str_array_cmd oc ic); asking ()
                    | Cd str  -> (do_command 5 str_array_cmd oc ic); asking ())
        in
        asking ()
    with
        exn -> Unix.close sock ; if ((ask_for_retry ()) == 0) then ()
        else client_func ()
;;

client_func ();;