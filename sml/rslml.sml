structure RSL =
    struct
	structure Int = Int;
	structure Real = Real;
	structure Text = String;
	structure Char = Char;
	structure Time = Time;
	structure Random = Random; 

	exception RSL_exception of string;

	exception swap of string;

        val start_time : Time.time ref = ref Time.zeroTime;

	fun set_time () = start_time := Time.now();

	val load_errs : string list ref = ref [];

	val exception_count = ref 0;

        fun inc_exception_count () = exception_count := !exception_count + 1;

	fun add_load_err s = load_errs := !load_errs @ [s];

        fun print_load_errs1 [] = ()
          | print_load_errs1 (s :: sl) =
	      (TextIO.print (s ^ "\n");
	       print_load_errs1 sl);

        fun print_load_errs () = print_load_errs1(!load_errs);

        fun print_error_count() =
	  (exception_count := !exception_count + length (!load_errs);
	   if !exception_count > 0 then
	     TextIO.print
	       (Int.toString (!exception_count) ^ " error(s)\n")
           else ();
	   exception_count := 0;
	   load_errs := []); 

	val Int_const_0 = Int.fromInt 0;
	val Int_const_1 = Int.fromInt 1;

	val Real_const_0 = Real.fromInt 0;

	fun C_not f x = not (f x);

	fun C_output prefix f x =
	    (TextIO.print prefix;
	     TextIO.print (f x);
	     TextIO.print "\n");
	    
	fun C_until (f, g) = (f (); while (C_not g) () do f ());

	fun fnt () = true;
    end;

signature RS_EQUAL =
    sig
	type t;
        val equ: t * t -> bool;
	val toString: t -> string;
	val toStringSafe: (unit -> t) -> string;
	val typeToString: unit -> string;
    end;

structure RT_Unit = 
    struct
        type t = unit;

	fun equ (x, y) = true;

	fun toString _ = "()";

	fun toStringSafe x = toString(x())
	  handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);

	fun typeToString () = "Unit";
    end;

structure RT_Bool =
    struct
        type t = bool;

	fun equ (x: bool, y: bool) = x = y;

	fun toString x = if x then "true" else "false";

	fun toStringSafe x = toString(x())
	  handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);

	fun typeToString () = "Bool";
    end;

structure RT_Int =
    struct
	type t = RSL.Int.int;

	fun fromLit s =
	  (* exponenent syntax accepted for reals *)
	  RSL.Real.trunc (valOf (RSL.Real.fromString s))
	  handle _ => raise RSL.RSL_exception ("Invalid integer literal " ^ s);

	fun equ (x, y) = x = y;
	    
	val R_add = RSL.Int.+;

	val R_sub = RSL.Int.-;

	val R_mul = RSL.Int.*;

	fun R_div (x, y) =
	  if equ (y, RSL.Int_const_0) then raise RSL.RSL_exception "Division by zero"
	  else RSL.Int.quot (x, y);

	fun R_mod (x, y) =
	  if equ (y, RSL.Int_const_0) then raise RSL.RSL_exception "Modulo zero"
	  else RSL.Int.rem (x, y);

	val R_neg = RSL.Int.~;

	val R_lt = RSL.Int.<;

	val R_le = RSL.Int.<=;

	val R_gt = RSL.Int.>;

	val R_ge = RSL.Int.>=;

	fun exp (x, y) =
	  if equ (y, RSL.Int_const_0) then RSL.Int_const_1
	    else R_mul (x, exp (x, R_sub (y, RSL.Int_const_1)));

        fun R_exp (x, y) =
	  if RSL.Int.< (y, RSL.Int_const_0) then raise RSL.RSL_exception ("Integer exponentiation with negative exponent " ^ RSL.Int.toString y)
	  else
	    if equ (x, RSL.Int_const_0) andalso equ (y, RSL.Int_const_0)
	    then raise RSL.RSL_exception "Cannot compute 0 ** 0"
	    else exp (x, y);

	val R_abs = RSL.Int.abs;

	fun R_real x = RSL.Real.fromLargeInt (RSL.Int.toLarge x);

	fun toString x =
	    if RSL.Int.< (x, RSL.Int_const_0) then "-" ^ (RSL.Int.toString (RSL.Int.abs x))
	    else RSL.Int.toString x;

	fun toStringSafe x = toString(x())
	  handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);

	fun typeToString () = "Int";
    end;

structure RT_Nat = RT_Int;

structure RT_Real =
    struct
	type t = RSL.Real.real;

	fun fromLit s = 
	    let 
		fun f (SOME x) = x
		  | f NONE = raise RSL.RSL_exception ("Invalid real literal " ^ s)
	    in f (RSL.Real.fromString s) end;

	fun equ (x, y) = RSL.Real.== (x, y);
	    
	val R_add = RSL.Real.+;

	val R_sub = RSL.Real.-;

	val R_mul = RSL.Real.*;

	fun R_div (x, y) =
	  if equ (y, RSL.Real_const_0) then raise RSL.RSL_exception "Division by zero"
	  else RSL.Real./ (x, y);

	val R_neg = RSL.Real.~;

	val R_lt = RSL.Real.<;

	val R_le = RSL.Real.<=;

	val R_gt = RSL.Real.>;

	val R_ge = RSL.Real.>=;

	val R_abs = RSL.Real.abs;

	fun R_int x = RSL.Real.trunc x;
	
	fun R_exp (x, y) =
	  if equ (x, RSL.Real_const_0) andalso RSL.Real.<= (y, RSL.Real_const_0)
	    then raise RSL.RSL_exception ("Zero raised to non-positive power " ^ RSL.Real.toString y)
	  else
	    if RSL.Real.< (x, RSL.Real_const_0) andalso not (equ (y, RSL.Real.realTrunc y))
	      then raise RSL.RSL_exception ("Negative number " ^ RSL.Real.toString x ^ " raised to non-integer power " ^ RSL.Real.toString y)
	    else RSL.Real.Math.pow (x, y); 

	fun toString x =
	    if RSL.Real.< (x, RSL.Real_const_0) then "-" ^ (RSL.Real.toString (RSL.Real.abs x))
	    else RSL.Real.toString x; 

	fun toStringSafe x = toString(x())
	  handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);

	fun typeToString () = "Real";
    end;

structure RT_Char =
    struct
	type t = char;

	fun equ (x, y) = x = y;
	    
	fun toString x = "'" ^ (Char.toString x) ^ "'";

	fun toStringSafe x = toString(x())
	  handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);

	fun typeToString () = "Char";
    end;

structure R'a_Set = 
    struct
	datatype 'a mt = E | S of 'a * 'a mt;

	fun fold f e s =
	    let
		fun addl (E, added) = added
		  | addl (S(y, ys), added) = addl (ys, f (y, added))
	    in
		addl (s, e)
	    end; 

	fun sizeOf s = fold (fn (y, n) => RSL.Int.+(n, RSL.Int_const_1)) RSL.Int_const_0 s;

        fun mem f (x, E) = false
	  | mem f (x, S(y, ys)) = f (x, y) orelse mem f (x, ys);

	fun hd E = raise RSL.RSL_exception "hd applied to empty set"
	  | hd (S(x, xs)) = x;
	    
	fun add f (x, xs) = if mem f (x, xs) then xs else S(x, xs);

	fun del f (x, xs) = fold (fn (y, ys) => if f (x, y) then ys else S(y, ys)) E xs

	fun addList f (xl, xs) = List.foldl (add f) xs xl;

	fun delList f (xl, xs) = List.foldl (del f) xs xl;

	fun subs f (E, s2) = true
	  | subs f (S(x, xs), s2) = mem f (x, s2) andalso subs f (xs, s2);

	fun subspro f (s1, s2) = RSL.Int.<((sizeOf s1), (sizeOf s2)) andalso subs f (s1, s2)

	fun sups f (s1, s2) = subs f (s2, s1)
	   
	fun supspro f (s1, s2) = subspro f (s2, s1)

	fun union f (E, s2) = s2
	  | union f (s1, E) = s1
	  | union f (s1, s2) = fold (fn (y, ys) => if mem f (y, s2) then ys else S(y, ys)) s2 s1

	fun diff f (E, _) = E
	  | diff f (s1, E) = s1
	  | diff f (s1, s2) = fold (fn (y, ys) => if mem f (y, s2) then ys else S(y, ys)) E s1

	fun inter f (E, _) = E
	  | inter f (_, E) = E
	  | inter f (s1, s2) = fold (fn (y, ys) => if mem f (y, s2) then S(y, ys) else ys) E s1

	fun fromList f xl = addList f (xl, E);

	fun toList s = fold (op::) [] s;

	fun all p E = true
	  | all p (S(x, xs)) = p x andalso all p xs;

        fun exists p E = false
	  | exists p (S(x, xs)) = p x orelse exists p xs;

        fun exists1 p E = false
	  | exists1 p (S(x, xs)) =
	    if p x then all (fn y => not (p y)) xs else exists1 p xs;

        fun choose p E = raise RSL.RSL_exception "Cannot select from empty set"
	  | choose p (S(x, xs)) =
	    if p x then x else choose p xs;

	fun equ f (s1, s2) = subs f (s1, s2) andalso subs f (s2, s1);

	fun toString f E = "{}"
	  | toString f (S(x, xs)) =
		"{" ^
		(fold (fn (y, str) => str ^ "," ^ f y) (f x) xs) ^
		"}";

	fun typeToString f = f () ^ "-set"; 

	fun uptoInt (x, y) = if RSL.Int.>(x, y) then E 
			     else S(x, uptoInt (RSL.Int.+(x, RSL.Int_const_1), y));

	(* fun uptoChar (x, y) = if RSL.Char.>(x, y) then E
			      else S(x, uptoChar (RSL.Char.succ x, y)); *)
    end;

structure R'a_List =
    struct
	type 'a mt = 'a list;

	fun hd [] = raise RSL.RSL_exception "hd applied to empty list"
	  | hd (x::_) = x;

	fun tl [] = raise RSL.RSL_exception "tl applied to empty list"
	  | tl (_::l) = l;

	fun ins (x, xl) = x::xl;

	fun foldl f e l = List.foldl f e l;

	fun foldr f e l = List.foldr f e l;

	fun sizeOf l = RSL.Int.fromInt (List.length l)

	val length = sizeOf;

        fun mem f (x, []) = false
	  | mem f (x, y::yl) = f (x, y) orelse mem f (x, yl);

	fun inds l = R'a_Set.uptoInt (RSL.Int_const_1, sizeOf l);

	fun elems f l = R'a_Set.fromList f l

	fun rev l = List.rev l;

	fun concat (l1, l2) = l1 @ l2;

	fun take n l = List.take (l, n);

	fun drop n l = List.drop (l, n);

	fun app [] y = raise RSL.RSL_exception "List applied to index outside index set"
	  | app (x::xl) y = if RT_Int.equ (y, RSL.Int_const_0) then x
			    else app xl (RT_Int.R_sub (y, RSL.Int_const_1));

	fun all p l = List.all p l;

        fun exists p l = List.exists p l;

        fun exists1 f p ([]) = false
	  | exists1 f p (x::xl) =
	    if p x
	    then all (fn y => not (p y) orelse f (x, y)) xl
	    else exists1 f p xl;

        fun choose p [] = raise RSL.RSL_exception "Cannot select from empty list"
	  | choose p (x::xl) =
	    if p x then x else choose p xl;

	fun equ f ([], []) = true
	  | equ f ([], _) = false
	  | equ f (_, []) = false
	  | equ f (x::xl, y::yl) = f (x, y) andalso equ f (xl, yl)

	fun toString f ([]) = "<..>"
	  | toString f (x::xl) =
	        "<." ^
	        (foldl (fn (y, z) => z ^ "," ^ (f y)) (f x) xl) ^
	        ".>";
	
	fun typeToString f = (f ()) ^ "-list";	

	fun uptoInt (x, y) = if RSL.Int.>(x, y) then [] 
			     else x::(uptoInt (RSL.Int.+(x, RSL.Int_const_1), y));

	(* fun uptoChar (x, y) = if RSL.Char.>(x, y) then []
			      else x::(uptoChar (RSL.Char.succ x, y)); *)
    end;

structure R'a_Map =
    struct
	datatype ('a, 'b) mt = E | M of 'a * 'b * ('a, 'b) mt;

	fun fold f e m = 
	    let
		fun addl (E, added) = added
		  | addl (M(xd, xr, xm), added) = addl (xm, f (xd, xr, added))
	    in
		addl (m, e)
	    end;
	
	type 'a eqt = 'a * 'a -> bool;

	fun equ (f: 'a eqt, g: 'b eqt) (m1: ('a, 'b) mt, m2: ('a, 'b) mt) = 
	    let
		fun s(E, _) = true
		  | s(_, E) = false
		  | s(M(x, y, m), M(x', y', m')) =
		    (f(x, x') andalso g(y, y') andalso s(m, m')) orelse
		    (s(M(x, y, E), m') andalso s(m, M(x', y', m')))
	    in
		s(m1, m2) andalso s(m2, m1)
	    end

	fun dommem f (xd, E) = false
	  | dommem f (xd, M(yd, _, ym)) = f (xd, yd) orelse dommem f (xd, ym);

	fun mem f (x, m) = dommem f (x, m);

	fun ranmem f (xr, E) = false
	  | ranmem f (xr, M(_, yr, ym)) = f (xr, yr) orelse ranmem f (xr, ym);

        fun disjoint f (E, _) = true
	  | disjoint f (_, E) = true
	  | disjoint f (M(xd, _, xm), m) =
	     not (dommem f (xd, m)) andalso disjoint f (xm, m);

	fun hd E = raise RSL.RSL_exception "hd applied to empty map"
	  | hd (M(xd, _, _)) = xd;

	fun restrict f m = fold (fn (xd, xr, e) => if f xd then M(xd, xr, e) else e) E m;

	fun app f E d = raise RSL.RSL_exception "Map applied to value outside domain"
	  | app f (M(xd, xr, xm)) d = if f (d, xd) then xr else app f xm d;

	fun override f (E, m2) = m2
	  | override f (m1, E) = m1
	  | override f (m1, m2) = fold (fn (yd, yr, ym) => if dommem f (yd, m2) then ym else M(yd, yr, ym)) m2 m1;

	fun del f (xd, m) = fold (fn (yd, yr, ym) => if f (xd, yd) then ym 
						     else M(yd, yr, ym)) E m;

	fun delList f (l, m) = List.foldl (del f) m l;

	fun add (fd, fr) ((xd, xr), m) =
	  if dommem fd (xd, m) 
	  then
	    if fr (xr, app fd m xd) then m
	    else raise RSL.RSL_exception "Nondeterministic enumerated map"
	  else M(xd, xr, m);

	fun addList (fd, fr) (l, m) = List.foldl (fn ((xd, xr), xm) => add (fd, fr) ((xd, xr), xm)) m l;

	fun sizeOf m = fold (fn (_, _, n) => RSL.Int.+(n, RSL.Int_const_1)) RSL.Int_const_0 m;

	fun dom f m = fold (fn (xd, _, xs) => R'a_Set.add f (xd, xs)) R'a_Set.E m;

	fun ran f m = fold (fn (_, xr, xs) => R'a_Set.add f (xr, xs)) R'a_Set.E m;

	fun fromList (fd, fr) xl = addList (fd, fr) (xl, E);

	fun compose f m E = E
	  | compose f m (M(xd, xr, xm)) =
	    if dommem f (xr, m) then M(xd, (app f m xr), (compose f m xm))
	    else raise RSL.RSL_exception "Maps do not compose";
	    
	fun all p E = true
	  | all p (M(x, _, m)) = p x andalso all p m;

        fun exists p E = false
	  | exists p (M(x, _, m)) = p x orelse exists p m;

        fun exists1 p E = false
	  | exists1 p (M(x, _, m)) =
	    if p x then all (fn y => not (p y)) m else exists1 p m;

        fun choose p E = raise RSL.RSL_exception "Cannot select from empty map"
	  | choose p (M(xd, _, xm)) =
	    if p xd then xd else choose p xm;

	fun toString (f, g) E = "[]"
	  | toString (f, g) (M(xd, xr, m)) = 
	    let
		fun pair (r, d) = (f r) ^ " +> " ^ (g d)
	    in
		"[" ^
		(fold (fn (yd, yr, z) => z ^ ", " ^ (pair (yd, yr))) (pair (xd, xr)) m) ^
		"]"
	    end;

	fun typeToString (f, g) = (f ()) ^ " -m-> " ^ (g ());
    end;

structure R'a_Comprehend =
    struct
        fun compss f g p R'a_Set.E = R'a_Set.E
	  | compss f g p (R'a_Set.S(x, xs)) =
	    if p x then R'a_Set.add f ((g x), (compss f g p xs))
	    else compss f g p xs;

        fun compls f g p [] = R'a_Set.E
	  | compls f g p (x::xl) =
	    if p x then R'a_Set.add f ((g x), (compls f g p xl))
	    else compls f g p xl;

        fun compms f g p R'a_Map.E = R'a_Set.E
	  | compms f g p (R'a_Map.M(xd, _, xm)) =
	    if p xd then R'a_Set.add f ((g xd), (compms f g p xm))
	    else compms f g p xm;

        fun compll g p [] = []
	  | compll g p (x::xl) =
	    if p x then (g x)::(compll g p xl)
	    else compll g p xl;

        fun compsm (fd, fr) g p R'a_Set.E = R'a_Map.E
	  | compsm (fd, fr) g p (R'a_Set.S(x, xs)) =
	    if p x then R'a_Map.add (fd, fr) ((g x), (compsm (fd, fr) g p xs))
	    else compsm (fd, fr) g p xs;

        fun complm (fd, fr) g p [] = R'a_Map.E
	  | complm (fd, fr) g p (x::xl) =
	    if p x then R'a_Map.add (fd, fr) ((g x), (complm (fd, fr) g p xl))
	    else complm (fd, fr) g p xl;

        fun compmm (fd, fr) g p R'a_Map.E = R'a_Map.E
	  | compmm (fd, fr) g p (R'a_Map.M(xd, _, xm)) =
	    if p xd then R'a_Map.add (fd, fr) ((g xd), (compmm (fd, fr) g p xm))
	    else compmm (fd, fr) g p xm;
    end;

functor RT_Set (structure Elem: RS_EQUAL) =
    struct
        structure Elem = Elem;
	open R'a_Set;

	type t = Elem.t mt; 
	
        val empty:t = E;

	fun R_sizeOf s = R'a_Set.sizeOf s;

	fun R_card s = sizeOf s;

        fun R_mem (x, y) = R'a_Set.mem Elem.equ (x, y);

	fun R_hd (x:t) = R'a_Set.hd x;

	fun R_add (x, xs) = R'a_Set.add Elem.equ (x, xs);

	fun R_addList (xl, xs) = R'a_Set.addList Elem.equ (xl, xs);

	fun R_del (x, xs) = R'a_Set.del Elem.equ (x, xs);

	fun R_delList (xl, xs) = R'a_Set.delList Elem.equ (xl, xs);

	fun R_subs (s1, s2) = R'a_Set.subs Elem.equ (s1, s2);

	fun R_subspro (s1, s2) = R'a_Set.subspro Elem.equ (s1, s2);

	fun R_sups (s1, s2) = R'a_Set.sups Elem.equ (s1, s2);
	   
	fun R_supspro (s1, s2) = R'a_Set.supspro Elem.equ (s1, s2);

	fun R_union (s1, s2) = R'a_Set.union Elem.equ (s1, s2);

	fun R_diff (s1, s2) = R'a_Set.diff Elem.equ (s1, s2);

	fun R_inter (s1, s2) = R'a_Set.inter Elem.equ (s1, s2);

	fun R_fromList xl = R'a_Set.fromList Elem.equ xl;

	fun R_toList s = R'a_Set.toList s;

        fun R_all p s = R'a_Set.all p s;

        fun R_exists p s = R'a_Set.exists p s;

        fun R_exists1 p s = R'a_Set.exists1 p s;

        fun R_choose p s = R'a_Set.choose p s;

	fun R_compss g p s = R'a_Comprehend.compss Elem.equ g p s;

	fun R_compls g p l = R'a_Comprehend.compls Elem.equ g p l;

	fun R_compms g p m = R'a_Comprehend.compms Elem.equ g p m;

	fun equ (s1, s2) = R'a_Set.equ Elem.equ (s1, s2);

	fun toString xs = R'a_Set.toString Elem.toString xs 

	fun toStringSafe x = toString(x())
	  handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);

	fun typeToString () = R'a_Set.typeToString Elem.typeToString; 
    end;

functor RT_List (structure Elem: RS_EQUAL) =
    struct
	structure Elem = Elem;
	open R'a_List;

	type t = Elem.t mt; 

	val empty:t = [];

	fun R_foldl f e l = R'a_List.foldl f e l;

	fun R_foldr f e l = R'a_List.foldr f e l;

	fun R_hd l = R'a_List.hd l;

	fun R_tl l = R'a_List.tl l;

	fun R_ins (x, xl) = R'a_List.ins(x, xl);

        fun R_mem (x, xl) = R'a_List.mem Elem.equ (x, xl);

	fun R_sizeOf l = R'a_List.sizeOf l;

	val R_length = R_sizeOf;

	fun R_inds l = R'a_List.inds l;

	fun R_elems l = R'a_List.elems Elem.equ l

	fun R_rev l = R'a_List.rev l;

	fun R_concat (l1, l2) = R'a_List.concat(l1, l2);

	fun R_take n l = R'a_List.take n l;

	fun R_drop n l = R'a_List.drop n l;

	fun toString x = R'a_List.toString Elem.toString x;
	
	fun R_app l n =
	  let val ind = RSL.Int.- (n, RSL.Int_const_1)
	  in
            if RSL.Int.>= (ind, RSL.Int_const_0)
	      andalso RSL.Int.< (ind, R'a_List.sizeOf l)
	    then R'a_List.app l ind
	    else raise RSL.RSL_exception
	      ("List " ^ toString l ^ " applied to non-index " ^ RSL.Int.toString n)
	  end;

        fun R_all p l = R'a_List.all p l;

        fun R_exists p l = R'a_List.exists p l;

        fun R_exists1 p l = R'a_List.exists1 Elem.equ p l;

        fun R_choose p l = R'a_List.choose p l;

	fun R_compll g p l = R'a_Comprehend.compll g p l;

	fun equ (x, y) = R'a_List.equ Elem.equ (x, y);

	fun toStringSafe x = toString(x())
	  handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);

	fun typeToString () = R'a_List.typeToString Elem.typeToString;	
    end;

structure RT_Text =
    struct
	type t = RT_Char.t list;

	fun equ (x, y) = x = y;
	    
	val empty:t = [];

	fun R_foldl f e l = R'a_List.foldl f e l;

	fun R_foldr f e l = R'a_List.foldr f e l;

	fun R_hd s = R'a_List.hd s;

	fun R_tl s = R'a_List.tl s;

	fun R_ins (x, xl) = R'a_List.ins(x, xl);

        fun R_mem (c, s) = R'a_List.mem equ (c, s);

	fun R_sizeOf s = R'a_List.sizeOf s;

	val R_length = R_sizeOf;

	fun R_inds s = R'a_List.inds s;

	fun R_elems s = R'a_List.elems equ s

	fun R_rev s = R'a_List.rev s;

	fun R_concat (s1, s2) = R'a_List.concat (s1, s2)

	fun R_take n l = R'a_List.take n l;

	fun R_drop n l = R'a_List.drop n l;

	fun toString x = "\"" ^ (implode x) ^ "\"";

	fun R_app s n =
	  let val ind = RSL.Int.- (n, RSL.Int_const_1)
	  in
            if RSL.Int.>= (ind, RSL.Int_const_0)
	      andalso RSL.Int.< (ind, R'a_List.sizeOf s)
	    then R'a_List.app s ind
	    else raise RSL.RSL_exception
	      ("Text " ^ toString s ^ " applied to non-index " ^ RSL.Int.toString n)
	  end;

        fun R_all p l = R'a_List.all p l;

        fun R_exists p l = R'a_List.exists p l;

        fun R_exists1 p l = R'a_List.exists1 equ p l;

        fun R_choose p l = R'a_List.choose p l;

	fun R_compll g p l = R'a_Comprehend.compll g p l;

	fun fromLit x = explode x;

	fun toStringSafe x = toString(x())
	  handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);

	fun typeToString () = "Text";
    end;

functor RT_Map(structure DomainElem: RS_EQUAL 
	       structure RangeElem: RS_EQUAL) =
    struct
	structure DomainElem = DomainElem;
	structure RangeElem = RangeElem;
	open R'a_Map;

	type t = (DomainElem.t, RangeElem.t) mt;

	val empty:t = E;

	fun fold f e m = R'a_Map.fold f e m;

	fun R_dommem (xd, m) = R'a_Map.dommem DomainElem.equ (xd, m);

	fun R_mem (xd, m) = R'a_Map.mem DomainElem.equ (xd, m);

	fun R_ranmem (xr, m) = R'a_Map.ranmem RangeElem.equ (xr, m);

	fun R_hd m = R'a_Map.hd m;

	fun R_restrToSet (m, s) = R'a_Map.restrict (fn x => R'a_Set.mem DomainElem.equ (x, s)) m;

	fun R_restrBySet (m, s) = R'a_Map.restrict (fn x => not (R'a_Set.mem DomainElem.equ (x, s))) m;

	fun toString m = R'a_Map.toString (DomainElem.toString, RangeElem.toString) m;

	fun R_app m d =
	  if R'a_Map.dommem DomainElem.equ (d, m)
	  then R'a_Map.app DomainElem.equ m d
	  else raise RSL.RSL_exception
	    ("Map "  ^ toString m ^ " applied to non-domain value " ^ DomainElem.toString d);
	
	fun R_override (m1, m2) = R'a_Map.override DomainElem.equ (m1, m2);

	fun R_union (m1, m2) =
	  if R'a_Map.disjoint DomainElem.equ (m1, m2) then R_override (m1, m2)
	  else raise RSL.RSL_exception
	    (toString m1 ^ " union " ^ toString m2 ^ " has non-disjoint domains");

	fun R_del (xd, m) = R'a_Map.del DomainElem.equ (xd, m);

	fun R_delList (l, m) = R'a_Map.delList DomainElem.equ (l, m);

	fun R_add (xd, xr, m) = R'a_Map.add (DomainElem.equ, RangeElem.equ) ((xd, xr), m);

	fun R_addList (l, m) = R'a_Map.addList (DomainElem.equ, RangeElem.equ) (l, m);

	fun R_sizeOf m = R'a_Map.sizeOf m;

	fun R_fromList m = R'a_Map.fromList (DomainElem.equ, RangeElem.equ) m;

	fun R_dom m = R'a_Map.dom DomainElem.equ m;

	fun R_ran m = R'a_Map.ran RangeElem.equ m;

	fun R_compose (ml:t, mr) = R'a_Map.compose DomainElem.equ ml mr;

        fun R_all p m = R'a_Map.all p m;

        fun R_exists p m = R'a_Map.exists p m;

        fun R_exists1 p m = R'a_Map.exists1 p m;

        fun R_choose p m = R'a_Map.choose p m;

	fun R_compsm g p s = R'a_Comprehend.compsm (DomainElem.equ, RangeElem.equ) g p s;

	fun R_complm g p l = R'a_Comprehend.complm (DomainElem.equ, RangeElem.equ) g p l;

	fun R_compmm g p m = R'a_Comprehend.compmm (DomainElem.equ, RangeElem.equ) g p m;

	fun equ (x, y) = R'a_Map.equ (DomainElem.equ, RangeElem.equ) (x, y);

	fun toStringSafe x = toString(x())
	  handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);

	fun typeToString () = R'a_Map.typeToString (DomainElem.typeToString, RangeElem.typeToString);
    end;

structure R'a_Fun = 
    struct
	type ('a, 'b) mt = 'a -> 'b;

	fun app f p = f p;

	fun equ (_, _) = raise RSL.RSL_exception "Cannot compute function equality";

	fun typeToString (f, arrow, g) = (f ()) ^ " " ^ arrow ^ " " ^ (g ());

	fun toString (f, arrow, g) = "fn: " ^ (typeToString (f, arrow, g))
    end;

functor RT_Fun(structure Param: RS_EQUAL
	       val arrow: string
	       structure Result: RS_EQUAL) =
    struct
	structure Param = Param;
	structure Result = Result;
	open R'a_Fun;

	type t = (Param.t, Result.t) mt;
	
	fun R_app f p = R'a_Fun.app f p;

	fun equ (x, y) = R'a_Fun.equ (x, y);

	fun toString x = R'a_Fun.typeToString (Param.typeToString, arrow, Result.typeToString);

	fun toStringSafe x = toString(x())
	  handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);

	fun typeToString () = R'a_Fun.typeToString (Param.typeToString, arrow, Result.typeToString);
    end;

structure R_int_choice =
    struct
      structure Int_set = RT_Set(structure Elem = RT_Int);

        val seed : Rand.rand ref = ref (Word31.fromInt(0));

        val first_call : bool ref = ref true;

        fun rnd(n) =
	  (if !first_call then
	    (seed :=
	       Word31.fromLargeInt(
		   Time.toMilliseconds(Time.-(Time.now(), !RSL.start_time)));
	     first_call := false)
	   else ();
	   seed := Rand.random(!seed);
	   Rand.range(0, n-1)(!seed));

        fun choice1(s, l, n, r, str) =
	  if Int_set.R_card s = n
	    then raise RSL.swap str
	      else if Int_set.R_mem(r, s) then choice1(s, l, n, (r+1) mod n, str)
	      else List.nth(l, r)() handle RSL.swap _ =>
		choice1(Int_set.R_add(r, s), l, n, (r+1) mod n, str);

        fun choice(l, str) =
	  let val n = length l in
	    choice1(Int_set.empty, l, n, rnd(n), str) end;

    end;

structure R_coverage =
  struct
    structure Region =
      struct
	type t = (RT_Int.t * RT_Int.t) * (RT_Int.t * RT_Int.t);

	fun equ (x:t, y:t) = RT_Int.equ(#1 (#1 x), #1 (#1 y)) andalso 
                             RT_Int.equ(#2 (#1 x), #2 (#1 y)) andalso 
                             RT_Int.equ(#1 (#2 x), #1 (#2 y)) andalso 
                             RT_Int.equ(#2 (#2 x), #2 (#2 y));
        
        fun toString (x:t) = "((" ^
                             (RT_Int.toString(#1 (#1 x) )) ^ "," ^
                             (RT_Int.toString(#2 (#1 x) )) ^ "),(" ^
                             (RT_Int.toString(#1 (#2 x) )) ^ "," ^
                             (RT_Int.toString(#2 (#2 x) )) ^
                             "))";
        
        fun toStringSafe x = toString(x())
          handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);
        
        fun typeToString () = "((" ^
                              (RT_Int.typeToString ()) ^ " >< " ^
                              (RT_Int.typeToString ()) ^ ") >< (" ^
                              (RT_Int.typeToString ()) ^ " >< " ^
                              (RT_Int.typeToString ()) ^
                              "))";
        
      end;

    structure Marks = RT_List(structure Elem = Region);

    structure RT_Text = RT_Text;

    structure FileMarks = RT_Map(structure DomainElem = RT_Text
			         structure RangeElem = Marks);

    val file_marks : FileMarks.t ref = ref FileMarks.empty;

    fun init () = file_marks := FileMarks.empty;

    fun add_region [] ((lb, cb), (le, ce)) = ((lb, cb), (le, ce))::[]
      | add_region (((lb1, cb1), (le1, ce1))::ms) ((lb, cb), (le, ce)) =
          if lb1 = lb andalso cb1 = cb then ((lb1, cb1), (le1, ce1))::ms
	  else ((lb1, cb1), (le1, ce1))::add_region ms ((lb, cb), (le, ce));

    (* cancel is of the beginning line and column of a region *)
    fun remove_pos [] (_, _) = []
      | remove_pos (((lb1, cb1), (le1, ce1))::ms) (l, c) =
          if lb1 = l andalso cb1 = c then ms
	  else ((lb1, cb1), (le1, ce1))::remove_pos ms (l, c);

    fun mark (f, b, e) =
      let val fm = !file_marks in
      let val marks =
	if FileMarks.R_mem(f, fm) then FileMarks.R_app fm f else [] in
      let val marks1 = add_region marks (b, e)
      in
        file_marks := FileMarks.R_override(fm, FileMarks.R_add(f, marks1, FileMarks.empty))
      end end end;
      
    (* cancel is of the beginning line and column of a region *)
    fun cancel (f, r) =
      let val fm = !file_marks in
      let val marks =
	if FileMarks.R_mem(f, fm) then FileMarks.R_app fm f else [] in
      let val marks1 = remove_pos marks r
      in
        file_marks := FileMarks.R_override(fm, FileMarks.R_add(f, marks1, FileMarks.empty))
      end end end;

    fun list_marked () = FileMarks.toString(!file_marks);

    fun regionToLisp (l, c) =
      "(rsl-lc-to-pos " ^ RT_Int.toString l ^ " " ^ RT_Int.toString c ^ ")";

    fun toLisp1 [] = ""
      | toLisp1 ((b, e)::l) =
          "(list " ^ regionToLisp b ^ " " ^ regionToLisp e ^ ") " ^ toLisp1 l;

    fun toLisp l = "(setq rsl-overlay-poss (list " ^ (toLisp1 l) ^ "))";

    fun save_marked1 fm =
	if FileMarks.equ (fm, FileMarks.empty)
	then ()
	else
	  let val file = FileMarks.hd fm;
	      val filename = implode file;
	      val lispfilename = filename ^ ".el";
	      val marks = FileMarks.R_app fm file;
	      val os = TextIO.openOut lispfilename
	  in if Marks.equ(marks, [])
	    then
	      (TextIO.output(os, "(setq rsl-overlay-poss nil)");
	       TextIO.closeOut os;
	       (*text of string must match coverage-message-string in rsltc.el*)
	       TextIO.print ("Complete expression coverage of " ^ filename ^ "\n"))
	    else
	      (TextIO.output(os, toLisp marks);
	       TextIO.closeOut os;
	       (*text of string must match coverage-message-string in rsltc.el*)
	       TextIO.print ("Unexecuted expressions in " ^ filename ^ "\n"));
	    save_marked1 (FileMarks.R_del (file, fm))
	  end;

    fun save_marked () = save_marked1 (!file_marks);

    end;

