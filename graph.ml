
open Types
open Graphics

type rectangle = (int*int*int*int*state);;

let default_size = 400;;

let draw_rectangle ((x,y,w,h,s):rectangle) =
draw_rect x y w h;
if s == A then begin
set_color blue;
fill_rect (x+1) (y+1) (w-2) (h-2);
end
else begin 
set_color white; 
fill_rect (x+1) (y+1) (w-2) (h-2);
end;
set_color black;;

let gen_to_array_rect (gen:generation) = 
let length = Array.length gen in
let array_rect = Array.make_matrix length length ((0,0,0,0,D):rectangle) in
for i = 0 to length-1 do
for j = 0 to length-1 do
array_rect.(i).(length-1-j) <- (i*(default_size/length),j*(default_size/length),(default_size/length),(default_size/length),gen.(i).(length-1-j));
done; 
done; 
array_rect;;

let rect_matrix_to_graph mat = 
let length = Array.length mat in
for i=0 to length-1 do
for j = 0 to length-1 do
draw_rectangle mat.(i).(j);
done;
done;;

let draw_gen (gen:generation) = rect_matrix_to_graph (gen_to_array_rect gen);;

let init gen = 
let size = (default_size/(Array.length gen))*(Array.length gen)+1 in 
open_graph (" "^(string_of_int size)^"x"^(string_of_int size)) ;
set_color black;
draw_gen gen;;

(*let tabs = Array.make_matrix 4 2 ((600,0,default_size/3,default_size/3):rectangle);;
Array.set tabs.(0) 0 (200,0);;
Array.set tabs.(0) 1 (200,600);;
Array.set tabs.(1) 0 (400,0);;
Array.set tabs.(1) 1 (400,600);;
Array.set tabs.(2) 0 (0,200);;
Array.set tabs.(2) 1 (600,200);;
Array.set tabs.(3) 0 (0,400);;
Array.set tabs.(3) 1 (600,400);;
let init () = 
set_window_title "blabla";
open_graph (" "^(string_of_int default_size)^"x"^(string_of_int default_size)) ;
set_color black;
for n=0 to (Array.length tabs)-1 do 
draw_poly_line tabs.(n);
done;;

init ();;
*)
