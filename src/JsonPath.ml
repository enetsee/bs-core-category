
    (*** -- JsonPath expressions -- ***)  
    type  t = 
      | Root 
      | Current  
      | Aggr of  t * aggr *  t list
      | Children of t * string list
      | Indices of  t * int list
      | AllIndices of t
      | WildCard of t
      | DeepScan of t * string
      | Slice of  t * slice
      | Filter of t * filter

    (*** -- Slices -- ***)
    and slice = 
      | SliceFrom of int
      | SliceTo  of int 
      | SliceBetween of int * int

    (*** -- Filter expressions -- ***)
    and filter = 
      | Exists of t 
      | Rel of { path :  t ; op : relop ; lit: lit }
      | Regex of { path :  t ; pattern: string }
      | Set of { path :  t ; op : setop ; lits : lit list }
      (* size set-operator applied to strings *)    
      | Size of { path :  t ; str: string }
      | Empty of  t
      | And of filter * filter 
      | Or of filter * filter
      | Not of filter
      | Paren of filter

    and relop = 
      | EQ | NEQ | GT | GTEQ | LT | LTEQ

    and setop = 
      | IN | NIN | SUBSETOF | ANYOF | NONEOF | SIZE

    (*** -- Literals -- ***)  

    and lit = 
      | LitInt of int 
      | LitNum of float
      | LitStr of string
      | LitPath of  t

    (*** -- aggregate function -- ***)

    and aggr = 
      | FnMin
      | FnMax
      | FnAvg
      | FnStddev
      | FnLength
      | FnSum


    (*** -- Helpers -- ***)  

    (*** -- Top-level path -- ***)

    let atRoot = Root 

    let atCurrent = Current

    (*** -- Aggregate functions -- ***)

    let minOf ?(paths=[]) path = Aggr(path,FnMin,paths)

    let maxOf ?(paths=[]) path = Aggr(path,FnMax,paths)

    let sumOf ?(paths=[]) path = Aggr(path,FnSum,paths)

    let avgOf ?(paths=[]) path = Aggr(path,FnAvg,paths)

    let stddevOf ?(paths=[]) path = Aggr(path,FnStddev,paths)

    let lengthOf ?(paths=[]) path = Aggr(path,FnLength,paths)    

    (*** -- Path parts -- ***)

    let child pathPart ~name = Children (pathPart,[name])

    let children pathPart ~names = Children (pathPart,names)

    let allIndices part = AllIndices part 

    let index pathPart ~idx = Indices(pathPart,[idx])

    let indices pathPart ~idxs = Indices(pathPart,idxs)

    let wildCard pathPart = WildCard pathPart

    let deepScan pathPart ~name = DeepScan(pathPart,name)

    let sliceFrom pathPart ~lower =  Slice(pathPart,SliceFrom lower)

    let sliceTo pathPart ~upper =  Slice(pathPart,SliceTo upper)

    let sliceBetween pathPart ~lower ~upper =  Slice(pathPart,SliceBetween(lower,upper))  

    let filter pathPart ~f = Filter(pathPart,f)

    (*** -- Filters --- ***)

    let and_ x y = And(x,y)

    let andMany  xs = 
      match xs with 
      | [] -> failwith "Empty list"
      | x::[] -> x
      | x::y::rest -> List.foldRight ~f:and_ rest ~init:(and_ x y) 

    let or_ x y = Or(x,y)

    let orMany  xs = 
      match xs with 
      | [] -> failwith "Empty list"
      | x::[] -> x
      | x::y::rest -> List.foldRight ~f:or_ rest ~init:(or_ x y) 

    let not_ x = Not(x)
    let paren x = Paren(x)

    let litInt n = LitInt n 
    let litNum n = LitNum n
    let litStr n = LitStr n 
    let litPath p = LitPath p

    let eq path ~lit = Rel {path;op=EQ;lit}

    let eqInt path n = eq path ~lit:(LitInt n)
    let eqNum path n = eq path ~lit:(LitNum n)
    let eqStr path n = eq path ~lit:(LitStr n)
    let eqPath path n = eq path ~lit:(LitPath n)

    let neq path ~lit = Rel {path;op=NEQ;lit}
    let neqInt path n = neq path ~lit:(LitInt n)
    let neqNum path n = neq path ~lit:(LitNum n)
    let neqStr path n = neq path ~lit:(LitStr n)
    let neqPath path n = neq path ~lit:(LitPath n)

    let gt path ~lit = Rel {path;op=GT;lit}
    let gtInt path n = gt path ~lit:(LitInt n)
    let gtNum path n = gt path ~lit:(LitNum n)
    let gtStr path n = gt path ~lit:(LitStr n)
    let gtPath path n = gt path ~lit:(LitPath n)

    let gteq path ~lit = Rel {path;op=GTEQ;lit}
    let gteqInt path n = gteq path ~lit:(LitInt n)
    let gteqNum path n = gteq path ~lit:(LitNum n)
    let gteqStr path n = gteq path ~lit:(LitStr n)
    let gteqPath path n = gteq path ~lit:(LitPath n)

    let lt path ~lit = Rel {path;op=LT;lit}
    let ltInt path n = lt path ~lit:(LitInt n)
    let ltNum path n = lt path ~lit:(LitNum n)
    let ltStr path n = lt path ~lit:(LitStr n)
    let ltPath path n = lt path ~lit:(LitPath n)


    let lteq path ~lit = Rel {path;op=LTEQ;lit}
    let lteqInt path n = lteq path ~lit:(LitInt n)
    let lteqNum path n = lteq path ~lit:(LitNum n)
    let lteqStr path n = lteq path ~lit:(LitStr n)
    let lteqPath path n = lteq path ~lit:(LitPath n)



    let regex path ~pattern = Regex {path;pattern}

    let in_ path ~lits = Set{path;op=IN;lits}    
    let subsetOf path ~lits = Set{path;op=SUBSETOF;lits}
    let anyOf path ~lits = Set{path;op=ANYOF;lits}
    let noneOf path ~lits = Set{path;op=NONEOF;lits}
    let notIn path ~lits = Set{path;op=NIN;lits}
    let sameSize path ~lits = Set{path;op=SIZE;lits}      
    let isEmpty path = Empty path 
    let sameSizeStr path ~str = Size{path;str}

    (*** -- Pretty printers -- ***)
    let ppAggr ppf = function 
      | FnMin -> Fmt.string ppf "min"
      | FnMax -> Fmt.string ppf "max"
      | FnAvg -> Fmt.string ppf "avg"
      | FnStddev -> Fmt.string ppf "stddev"
      | FnLength -> Fmt.string ppf "length"
      | FnSum -> Fmt.string ppf "sum"

    let ppRelOp ppf = function
      | EQ -> Fmt.string ppf "==" 
      | NEQ -> Fmt.string ppf "!=" 
      | GT -> Fmt.string ppf ">" 
      | GTEQ -> Fmt.string ppf ">=" 
      | LT -> Fmt.string ppf "<" 
      | LTEQ -> Fmt.string ppf "<=" 

    let ppSetOp ppf = function
      | IN -> Fmt.string ppf "in" 
      | NIN -> Fmt.string ppf "nin" 
      | SUBSETOF -> Fmt.string ppf "subsetof" 
      | ANYOF -> Fmt.string ppf "anyof" 
      | NONEOF -> Fmt.string ppf "noneof" 
      | SIZE -> Fmt.string ppf "size" 



    let ppSlice ppf = function
      | SliceFrom n -> Fmt.pf ppf "[%i:]" n
      | SliceTo n -> Fmt.pf ppf "[:%i]" n
      | SliceBetween(m,n) -> Fmt.pf ppf "[%i:%i]" m n

    let rec ppFilter ppf = function
      | Exists path -> 
        pp ppf path
      | Rel { path ; op : relop ; lit } -> 
        Fmt.pf ppf "%a %a %a" pp path ppRelOp op ppLit lit      
      | Regex { path ; pattern } ->
        Fmt.pf ppf "%a =~ %s" pp path pattern      
      | Set { path ; op ; lits } ->
        Fmt.pf ppf "%a %a %a" pp path ppSetOp op Fmt.(list ~sep:comma ppLit) lits      
      | Size { path; str} -> 
        Fmt.pf ppf "%a size %s" pp path str      
      | Empty path -> Fmt.pf ppf "empty %a" pp path       
      | And (e,f) -> Fmt.pf ppf "%a && %a" ppFilter e ppFilter f
      | Or (e,f) -> Fmt.pf ppf "%a || %a" ppFilter e ppFilter f
      | Not ((Paren _) as f) -> Fmt.pf ppf "!%a"  ppFilter f
      | Not f -> Fmt.pf ppf "!%a" Fmt.(parens ppFilter) f
      | Paren f -> Fmt.pf ppf "%a" Fmt.(parens ppFilter) f

    and ppLit ppf = function 
      | LitStr str -> Fmt.pf ppf "%a" Fmt.(quote ~mark:"'" string) str
      | LitInt n -> Fmt.int ppf n
      | LitNum n -> Fmt.float ppf n
      | LitPath p -> pp ppf p

    and pp ppf = function
      | Root  -> Fmt.string ppf "$" 
      | Current  -> Fmt.string ppf "@" 
      | Aggr (path,aggr,paths) -> Fmt.pf ppf "%a.%a%a" pp path ppAggr aggr Fmt.(parens @@ list ~sep:comma pp) paths
      | Children (part,x::[]) -> Fmt.pf ppf "%a.%s" pp part x
      | Children (part,xs) -> Fmt.pf ppf "%a.%a" pp part Fmt.(brackets @@ list ~sep:comma string) xs
      | AllIndices part -> Fmt.pf ppf "%a[*]" pp part    
      | Indices (part,xs) -> Fmt.pf ppf "%a.%a" pp part Fmt.(brackets @@ list ~sep:comma int) xs     
      | WildCard part -> Fmt.pf ppf "%a.*" pp part
      | DeepScan(part,desc) -> Fmt.pf ppf "%a..%s" pp part desc
      | Slice(part,slice) -> Fmt.pf ppf "%a.%a" pp part ppSlice slice
      | Filter(part,filter) -> Fmt.pf ppf "%a[?%a]" pp part  Fmt.(parens ppFilter) filter
  
  module RefPath = struct
    (*** -- Unambiguous JsonPath expressions -- ***)  
    type  t = 
      | Root   
      | Child of t * string 
      | Index of  t * int       
      | DeepScan of t * string      
  end
  
