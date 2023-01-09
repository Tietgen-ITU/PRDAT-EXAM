# Programmer som data - Eksamen
Andreas Nicolaj Tietgen
anti@itu.dk


Jeg erklærer hermed at jeg selv har lavet hele denne eksamensbesvarelse uden hjælp fra andre.

## Opgave 1

### 1. 
For at kunne udskrive numbers er følgende blevet skrevet.
```F#
Every (Write (numbers));;
```
Det giver nedenstående output:

![](2023-01-09-09-40-17.png)

### 2.
For at skrive tal fra `numbers` som er større end 10 så kan følgende blive skrevet:
```F#
Every(Write(Prim("<", CstI (10), numbers)));;
```
Det giver nedenstående output:

![](2023-01-09-09-41-06.png)

### 3. 
For at kunne udskrive de tal som er større end for sekvensen af numbers kan følgende blive skrevet:
```F#
(Every (Write (Prim("<", numbers, Seq(Write(CstS("\n")), numbers)))));;
```
Det giver følgende output i terminalen:

![](2023-01-09-08-23-18.png)

### 4.
For at løse opgaven antager jeg at vi har med et alfabet som kun har store bogstaver og som kun indeholder ASCII bogstaver fra A-Z.

Koden for at løse dette er således:
```F#
let rec eval (e : expr) (cont : cont) (econt : econt) = 
    match e with
    | ...
    | FromToChar (c1, c2) -> 

      // Create loop function to provide chars to the continuation function  
      let rec loop = function
        | c :: cs -> cont (Str (c.ToString())) (fun () -> loop cs) 
        | [] -> econt ()

      // Create a function that checks that the char, c, is between ch1 and ch2
      let charIsBetweenInclusive (c, ch1, ch2) =
        let cv = int(c)
        let cv1 = int(ch1)
        let cv2 = int(ch2)

        cv >= cv1 && cv <= cv2
      
      // Check that c2 is greater than or equal to c1
      match (System.Char.ToUpper(c1), System.Char.ToUpper(c2)) with
      | ch1, ch2 when int(c2) - int(c1) < 0 -> Str "c2 is not greater than c1"
      | ch1, ch2 -> 
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ".ToCharArray() 
        |> List.ofArray 
        |> List.filter (fun c -> charIsBetweenInclusive (c, ch1, ch2))
        |> loop
```

### 5.
For at løse dette eksempel er følgende tilføjet til evalueringen af `Prim`:
```F#
let rec eval (e : expr) (cont : cont) (econt : econt) = 
    match e with
    | ...
    | Prim(ope, e1, e2) -> 
      eval e1 (fun v1 -> fun econt1 ->
          eval e2 (fun v2 -> fun econt2 -> 
              match (ope, v1, v2) with
              | ...
              | ("<", Str s1, Str s2) -> 
                if s1 < s2 then
                  cont (Str s2) econt2
                else 
                  econt2 ()
              econt1)
          econt
```

Det har givet følgende resultat når det bliver kørt i terminalen:

![](2023-01-09-09-34-29.png)

### 6.
Jeg har skrevet følgende udtryk:
```
Every(Write(Prim("<", CstS "G", chars)));;
```

Dette giver nedenstående resultat:
![](2023-01-09-09-39-25.png)