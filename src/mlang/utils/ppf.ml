(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2026 DGFiP - INRIA                                      *)
(*                                                                            *)
(* Ce programme est distribué sous la licence CeCILL-C: vous pouvez le        *)
(* redistribuer et/ou le modifier sous les contraintes de celle-ci.           *)
(*                                                                            *)
(* L'accessibilité au code source et les droits de copie, de modification et  *)
(* de redistribution qui découlent de ce contrat ont pour contrepartie de     *)
(* n'offrir aux utilisateurs qu'une garantie limitée et de ne faire peser sur *)
(* l'auteur du logiciel, le titulaire des droits patrimoniaux et les          *)
(* concédants successifs qu'une responsabilité restreinte.                    *)
(*                                                                            *)
(******************************************************************************)

type structured_msg = { msg : string; spans : (string option * Pos.t) list }

let make ?(spans = []) msg = { msg; spans }

let fmake ?spans fmt = Format.kasprintf (make ?spans) fmt

module type S = sig
  val debug_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val var_info_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val error_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val warning_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val result_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val format : Format.formatter -> structured_msg -> unit

  val create_progress_bar : string -> (string -> unit) * (string -> unit)
end

module ANSITerminal = struct
  (** {2 Markers} *)

  (** Prints [[INFO]] in blue on the terminal standard output *)
  let var_info_marker () =
    ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.blue ] "[VAR INFO] "

  let time : float ref = ref (Unix.gettimeofday ())

  let initial_time : float ref = ref (Unix.gettimeofday ())

  let time_marker () =
    let new_time = Unix.gettimeofday () in
    let old_time = !time in
    time := new_time;
    let delta = (new_time -. old_time) *. 1000. in
    if delta > 100. then
      ANSITerminal.printf
        [ ANSITerminal.Bold; ANSITerminal.black ]
        "[TIME] %.0f ms\n" delta

  (** Prints [[DEBUG]] in purple on the terminal standard output as well as
      timing since last debug *)
  let debug_marker () =
    if !Config.display_time then time_marker ();
    ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.magenta ] "[DEBUG] "

  (** Prints [[ERROR]] in red on the terminal error output *)
  let error_marker () =
    ANSITerminal.eprintf [ ANSITerminal.Bold; ANSITerminal.red ] "[ERROR] "

  (** Prints [[WARNING]] in yellow on the terminal standard output *)
  let warning_marker () =
    ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.yellow ] "[WARNING] "

  (** Prints [[RESULT]] in green on the terminal standard output *)
  let result_marker () =
    ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.green ] "[RESULT] "

  let clocks =
    Array.of_list [ "🕛"; "🕐"; "🕑"; "🕒"; "🕓"; "🕔"; "🕕"; "🕖"; "🕗"; "🕘"; "🕙"; "🕚" ]

  (** Prints [[🕛]] in blue on the terminal standard output *)
  let clock_marker i =
    let new_time = Unix.gettimeofday () in
    let initial_time = !initial_time in
    let delta = new_time -. initial_time in
    ANSITerminal.printf
      [ ANSITerminal.Bold; ANSITerminal.blue ]
      "[%s  %.1f s] "
      clocks.(i mod Array.length clocks)
      delta

  let create_progress_bar (task : string) : (string -> unit) * (string -> unit)
      =
    if !Config.no_nondet_display then (ignore, ignore)
    else
      let step_ticks = 5 in
      let ticks = ref 0 in
      let msg = ref task in
      let stop = ref false in
      let timer () =
        while true do
          if !stop then Thread.exit ();
          ticks := !ticks + 1;
          if !Config.display_time then clock_marker (!ticks / step_ticks);
          Format.printf "%s" !msg;
          flush_all ();
          flush_all ();
          ANSITerminal.erase ANSITerminal.Below;
          ANSITerminal.move_bol ();
          Unix.sleepf 0.05
        done
      in
      let _ = Thread.create timer () in
      ( (fun current_progress_msg ->
          msg := Format.sprintf "%s: %s\n" task current_progress_msg),
        fun finish_msg ->
          stop := true;
          result_marker ();
          Format.printf "%s: %s@." task finish_msg;
          ANSITerminal.erase ANSITerminal.Below;
          ANSITerminal.move_bol ();
          Format.printf "\n";
          time_marker () )

  (**{2 Printers}*)

  let debug_print ppf =
    ANSITerminal.erase ANSITerminal.Eol;
    if !Config.debug_flag then
      Format.kasprintf
        (fun str ->
          debug_marker ();
          Format.printf "%s\n@." str)
        ppf
    else Format.ifprintf Format.std_formatter ppf

  let var_info_print ppf =
    ANSITerminal.erase ANSITerminal.Eol;
    if !Config.var_info_flag then
      Format.kasprintf
        (fun str ->
          var_info_marker ();
          Format.printf "%s@." str)
        ppf
    else Format.ifprintf Format.std_formatter ppf

  let error_print ppf =
    ANSITerminal.erase ANSITerminal.Eol;
    Format.kasprintf
      (fun str ->
        error_marker ();
        Format.eprintf "%s@." str)
      ppf

  let warning_print ppf =
    ANSITerminal.erase ANSITerminal.Eol;
    if !Config.warning_flag then
      Format.kasprintf
        (fun str -> Format.printf "%a%s@." (fun _ -> warning_marker) () str)
        ppf
    else Format.ifprintf Format.std_formatter ppf

  let result_print ppf =
    ANSITerminal.erase ANSITerminal.Eol;
    Format.kasprintf
      (fun str -> Format.printf "%a%s@." (fun _ -> result_marker) () str)
      ppf

  let concat_with_line_depending_prefix_and_suffix (prefix : int -> string)
      (suffix : int -> string) (ss : string list) =
    match ss with
    | hd :: rest ->
        let out, _ =
          List.fold_left
            (fun (acc, i) s ->
              ( (acc ^ prefix i ^ s
                ^ if i = List.length ss - 1 then "" else suffix i),
                i + 1 ))
            ( (prefix 0 ^ hd ^ if 0 = List.length ss - 1 then "" else suffix 0),
              1 )
            rest
        in
        out
    | [] -> prefix 0

  let add_prefix_to_each_line (s : string) (prefix : int -> string) =
    concat_with_line_depending_prefix_and_suffix
      (fun i -> prefix i)
      (fun _ -> "\n")
      (String.split_on_char '\n' s)

  let indent_number (s : string) : int =
    try
      let rec aux (i : int) = if s.[i] = ' ' then aux (i + 1) else i in
      aux 0
    with Invalid_argument _ -> String.length s

  let format_with_style (styles : ANSITerminal.style list)
      (str : ('a, unit, string) format) =
    if !Config.plain_output (* can depend on a stylr flag *) then
      Printf.sprintf str
    else ANSITerminal.sprintf styles str

  let format_matched_line pos (line : string) (line_no : int) : string =
    let line_indent = indent_number line in
    let error_indicator_style = [ ANSITerminal.red; ANSITerminal.Bold ] in
    let sline = Pos.get_start_line pos in
    let eline = Pos.get_end_line pos in
    let line_start_col =
      if line_no = sline then Pos.get_start_column pos else 1
    in
    let line_end_col =
      if line_no = eline then Pos.get_end_column pos else String.length line + 1
    in
    let line_length = String.length line + 1 in
    line
    ^
    if line_no >= sline && line_no <= eline then
      "\n"
      ^
      if line_no = sline && line_no = eline then
        format_with_style error_indicator_style "%*s" (line_end_col - 1)
          (String.make (line_end_col - line_start_col) '^')
      else if line_no = sline && line_no <> eline then
        format_with_style error_indicator_style "%*s" (line_length - 1)
          (String.make (line_length - line_start_col) '^')
      else if line_no <> sline && line_no <> eline then
        format_with_style error_indicator_style "%*s%s" line_indent ""
          (String.make (line_length - line_indent) '^')
      else if line_no <> sline && line_no = eline then
        format_with_style error_indicator_style "%*s%*s" line_indent ""
          (line_end_col - 1 - line_indent)
          (String.make (line_end_col - line_indent) '^')
      else assert false (* should not happen *)
    else ""

  let format_lines pos lines =
    let filename = Pos.get_file pos in
    let sline = Pos.get_start_line pos in
    let eline = Pos.get_end_line pos in
    let blue_style = [ ANSITerminal.Bold; ANSITerminal.blue ] in
    let spaces = int_of_float (log10 (float_of_int eline)) + 1 in
    let lines =
      List.mapi (fun i line -> format_matched_line pos line (i + sline)) lines
    in
    format_with_style blue_style "%*s--> %s\n%s" spaces "" filename
      (add_prefix_to_each_line
         (Printf.sprintf "\n%s" (String.concat "\n" lines))
         (fun i ->
           let cur_line = sline + i - 1 in
           if
             cur_line >= sline
             && cur_line <= sline + (2 * (eline - sline))
             && cur_line mod 2 = sline mod 2
           then
             format_with_style blue_style "%*d | " spaces
               (sline + ((cur_line - sline) / 2))
           else if cur_line >= sline && cur_line < sline then
             format_with_style blue_style "%*d | " spaces cur_line
           else if
             cur_line <= sline + (2 * (eline - sline)) + 1
             && cur_line > sline + (2 * (eline - sline)) + 1
           then
             format_with_style blue_style "%*d | " spaces
               (cur_line - (eline - sline + 1))
           else format_with_style blue_style "%*s | " spaces ""))

  let retrieve_loc_text (pos : Pos.t) : string =
    let filename = Pos.get_file pos in
    if filename = "" then "No position information"
    else
      let lines =
        match !Config.filesystem with
        | Contents filemap -> begin
            match StrMap.find_opt filename filemap with
            | None -> failwith "Pos error"
            | Some contents ->
                let lines = String.split_on_char '\n' contents in
                [ List.nth lines (Pos.get_start_line pos - 1) ]
          end
        | Local ->
            let get_lines =
              match File.open_file_for_text_extraction pos with
              | exception Sys_error _ ->
                  Format.ksprintf failwith
                    "File not found for displaying position : %S" filename
              | get_lines -> get_lines
            in
            get_lines 1
      in
      format_lines pos lines

  let format fmt { msg; spans } =
    Format.fprintf fmt "%s%s%s%s" msg
      (if spans = [] then "" else "\n\n")
      (String.concat "\n\n"
         (List.map
            (fun (msg, pos) ->
              Printf.sprintf "%s%s"
                (match msg with None -> "" | Some msg -> msg ^ "\n")
                (retrieve_loc_text pos))
            spans))
      (if spans = [] then "" else "\n")
end

module GNU = struct
  include ANSITerminal

  let format fmt { msg; spans } =
    if spans = [] then Format.fprintf fmt "%s\n" msg
    else
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_newline fmt ())
        (fun fmt (pos_msg, pos) ->
          Format.fprintf fmt "%a: %s %a\n" Pos.format_gnu pos msg
            (fun fmt pos_msg ->
              match pos_msg with
              | None -> ()
              | Some pos_msg -> Format.fprintf fmt "[%s]" pos_msg)
            pos_msg)
        fmt spans
end

let logger_select () =
  match !Config.message_format with
  | GNU -> (module GNU : S)
  | ANSI -> (module ANSITerminal : S)

let error_print ppf =
  let module L = (val logger_select ()) in
  L.error_print ppf

let warning_print ppf =
  let module L = (val logger_select ()) in
  L.warning_print ppf

let debug_print ppf =
  let module L = (val logger_select ()) in
  L.debug_print ppf

let result_print ppf =
  let module L = (val logger_select ()) in
  L.result_print ppf

let error_str : string -> unit = error_print "%s"

let warning_str : string -> unit = warning_print "%s"

let debug_str : string -> unit = debug_print "%s"

let result_str : string -> unit = result_print "%s"

let create_progress_bar ppf =
  let module L = (val logger_select ()) in
  L.create_progress_bar ppf

let format_structured_message ppf =
  let module L = (val logger_select ()) in
  L.format ppf
