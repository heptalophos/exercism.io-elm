module ComplexNumbers exposing
    ( Complex
    , abs
    , add
    , conjugate
    , div
    , exp
    , fromPair
    , fromReal
    , imaginary
    , mul
    , real
    , sub
    )

type Complex = Complex Float Float

fromPair : ( Float, Float ) -> Complex
fromPair pair =
    let (re, im) = pair in Complex re im

fromReal : Float -> Complex
fromReal re = 
    Complex re 0.0

real : Complex -> Float
real z =
    let (Complex re _) = z in re
    

imaginary : Complex -> Float
imaginary z =
    let (Complex _ im) = z in im

conjugate : Complex -> Complex
conjugate z = 
    let (Complex re im) = z in Complex re -im
    
abs : Complex -> Float
abs z = 
    let (Complex re im) = z in (re ^ 2 + im ^ 2) ^ 0.5    

add : Complex -> Complex -> Complex
add z1 z2 =
    let
        (Complex r1 i1) = z1
        (Complex r2 i2) = z2
    in
        Complex (r1 + r2) (i1 + i2)
           

sub : Complex -> Complex -> Complex
sub z1 z2 =
    let
        (Complex r1 i1) = z1
        (Complex r2 i2) = z2
    in
        add (Complex r1 i1) (Complex -r2 -i2)


mul : Complex -> Complex -> Complex
mul z1 z2 =
    let
        (Complex r1 i1) = z1
        (Complex r2 i2) = z2
    in
        Complex (r1 * r2 - i1 * i2) (r1 * i2 + r2 * i1)


div : Complex -> Complex -> Complex
div z1 z2 =
    let
        reducedConjugate z =
            let
                d = (z |> abs) ^ 2
                (Complex r i) = z
            in
                Complex (r / d) -(i / d)
    in
        mul z1 (reducedConjugate z2)

    


exp : Complex -> Complex
exp z =
    
    let
        (Complex re im) = z
    in
        mul (Complex (e ^ re) 0) (Complex (cos im) (sin im))   
