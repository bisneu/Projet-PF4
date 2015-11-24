(*parce que ma teub est plus grosse *)
open Parser;;
let fichier = open_in (Sys.argv.(1));;

let arg = parse fichier;;
