/* primitives --------------------------------------------- */
(2 + 4 * 10) == 42;
0 == false;
1 == true;
2 != true;
2 != false;
0 != true;
1 != false;
false === false;
true === true;
42 === 42;
//0 === false; ~> erreur de typage détéctée à l'exécution
//1 !== true;  ~> erreur de typage détéctée à l'exécution
(2 < 3 === true) == true;
(2 > 3 === true) == false;
(2 <= 3 === true) == true;
(2 >= 3 === true) == false;
(3 <= 3 === true) == true;
(3 >= 3 === true) == true;
1 < 2 === 1 > 2 === false === true;
5 ** 3 === 125;
[1,5];
[1,5].car === 1;
[1,5].cdr === 5;
/* opérateur ternaire */
(true?1:2) === 1;
(false?1:2) === 2;
(false?1-6:2+3) === 5;
/* bloc --------------------------------------------- */
{
  let x = 3;
  x === 3;
}
// x; erreur à la compilation ~>  ==> Not in scope : x

/* affectation --------------------------------------------- */
{
  let x = 3;
  x = 2;
  x === 2;
  x += 40;
  x === 42;
  x *= 2;
  x === 84;
  x /= 2;
  x === 42;
  x -= 2;
  x === 40;
}

/* function --------------------------------------------- */

function fact(n){
  if (n<2) {
    return 1;
  }
  else {
    return n * fact(n-1);
  }
}

fact(6) == 720;

/* déclaration dans un bloc --------------------- */
{
  function fact_glob(n){ return(n<2)?n:n*fact_glob(n-1); }
  var glob = fact_glob(5);
}
glob === 120;
fact_glob(5)===120;

/* fermeture --------------------- */
var f = lambda(x) { return lambda (y) { return x + y;};};
var g = f(2);
g(3) === 5;

function h(x) { return lambda (y) { return x + y; };};
let k = f(2);
k(3) === 5;


/* while ----------------------- */
let wfact = lambda (n){
              var i = 1;
              var r = 1;
              while (i <= n){
                r *= i;
                i += 1;
              }
              return r;
          };
wfact(6) === 720;

//var www = 1;
/* ~> bug très étrange : "Assertion failed: (n<varray->top), function varray_set_at, file varray.c, line 101." */

/* do while ----------------------- */

let dd = 1;
do {
  dd = 2;
} while (false);
dd === 2;
do {
  dd += 1;
} while (dd < 10);
dd === 10;

/* bug ! ----------------------- */
let z = lambda (n){
         let i = 1;
         return i;
         /* ~> De par la manière dont est compilé le Let,
               il y'a un "PUSH UNIT ; RETURN" implicite,
               qui masque la valeur de retour de la fonction !!! */
    };
// z === 1; // donc ~> Unable to apply eq with types: 3 and 1
