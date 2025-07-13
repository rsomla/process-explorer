:- module(forth,[
  forth//1, forth//2
]).

:- multifile forth:verb/2, forth:verb//2, forth:verb//1.

@=> use_verb +Ctx +V -I

TODO: forth:verb/2

@-> =(Ctx,V,I)  ? {forth:verb(V,FF)}
  , forth_(Ctx,FF,I)
@-> =(_,V,I) ? forth:verb(V,I)
@-> =(_,V,{}) forth:verb(V)



:- begin_tests(forth). %, blocked(refactoring)).

@+= test +T
@-- =(6)
  , exec([2,@(brk),3],St,I)
  , assertion(St == [2])
  , assertion(I == brk-[3])

@== run +St0 +CC -St
@-- 
  , exec(St0:CC,St)
  , maplist([X,{X}]>>true,St0,CC0)
  , reverse(CC0,CC1)
  , append(CC1,CC,CC2)
  , exec(CC2,St2)
  , assertion(St == St2)

@+= test +T
@-- =(4)
  , run([a,b],[dup(1),$1,rm(1)], St)
  , assertion(St == [a,b])
@-- =(3)
  , run([a,b],[dup(1)], St)
  , assertion(St == [b,a,b])
@-- =(2)
  , run([a,b],[$1], St)
  , assertion(St == [b,a])
@-- =(1)
  , run([a,b],[rm], St)
  , assertion(St == [b])

:- end_tests(forth).

% :- initialization run_tests(forth).

-----------------------------------------------

@== exec +C  @- exec(C,_)

@== exec +C -St

Stack $St$ results from execution of Forth code $C$.

If $C$ is of the form $St:C$ then code $C$ is executed with initial stack $St$. Otherwise it is executed with initial empty stack.

Exception $@(I)$ is thrown if code execution was interrupted by $I$. Code $C$ is the continuation after interrupt.

@-- exec(C, St, I)
  , ( I={} -> true ; throw(@(I)) )


@== exec +C0 -St -I

Execution of Forth code $C0$ was interrupted by $I$ at stack $St$.

If $C0$ is of the form $St0:C0$ then code $C0$ is executed with initial stack $St0$. Otherwise it is executed with initial empty stack.

Contionuation $C$ is of the form $St:C$ where $St$ is the stack and $C$ is Forth code to be executed next.

If $I={}$ then code has terminated without interruption, otherwise $I$ is a temr $I-C$ where $I$ is a term describing the interrupt and $C$ is code to continue execution after interrupt is handled.

@-- exec([],C0,St,I)

@== exec +Ctx +C0 -St -I
@-- =(Ctx, St0:C0, St, I)
  , phrase(forth_(Ctx,C0,I), St0, St)
@-- exec(Ctx, []:C0, St, I)

-----------------------------------------------

@=> forth +C
@-> forth(C,I)
  , { I={} ; I = done-_ ; throw(@(I)) }

@=> forth +C0 -I  @- forth_([],C0,I)


@=> forth_ +Ctx +C0 -I

Note: Context  Ctx  is currently not used.

@-> =(_, [], {})

@-> =(Ctx, [X|C0], I)
  , verb_(Ctx,X,I1)
  , (
    { I1 = {} } -> forth_(Ctx,C0,I) ;
    { I1 = I2-C1, append(C1,C0,CC), I = I2-CC }
  )

@-> =(Ctx, X, I)
  , forth_(Ctx,[X],I)


@=> verb_ +Ctx +X -I

@-> =(_, done,  done-[])
@-> =(_, break, break-[])
@-> =(_, @(I),  I-[])

Match top of stack against given pattern.

@-> =(_, match(X), I)
  , get(0,Y)
  , { X=Y -> I = {} ; I = bad_match(X,Y)-[] }

List is treated as Forth code to execute.

@-> =(_, [], {})
@-> =(Ctx, [F|FF], I)  forth_(Ctx, [F|FF], I)

  Note: Code  [[done]|GG]  will terminate immediately while  [[]|GG]  will continue as  GG.

@-> =(Ctx, if(Then,Else), I)
  , pop(0,X)
  , ( {is_false(X)} -> forth_(Ctx,Else,I) ; forth_(Ctx,Then,I) )
@-> =(Ctx, if(Then), I)
  , verb_(Ctx, if(Then,[]), I)

Call Forth code. Difference between this and direct execution is that verb  done  insie called code will terminate only that code, not the whole thing.

@-> =(Ctx, call: FF, I)
  , forth_(Ctx, FF, I0)
  , {
      (I0 = {} ; I0 = done-_) -> I={} ;
      I0=I1-CC, I = I1-[call: CC] % <1>
    }

  Note <1>: Continuation is executed inside the call.

@-> =(Ctx, loop: BB, I) verb_(Ctx, loop_(BB,BB), I)
@-> =(Ctx, loop_(FF,BB), I)
  , forth_(Ctx, FF, I0)
  , loop_(Ctx, I0, BB, I)

@-> =(_,X,{})  ? verb_(X)
@-> use_verb(Ctx,X,I) ! unknown_verb(X)


@=> loop_ +Ctx +I0 +BB -I

Continuation of loop with body  BB  after single iteration was interrupted by  I0. 

@-> =(Ctx, {}, BB, I)  verb_(Ctx, loop_(BB,BB), I)
@-> =(_, done-_, _, done-[])
@-> =(_, break-_, _, {})
@-> =(_, I-FF, BB, I-[loop_(FF,BB)])


@=> verb_ +X

@-> =(eq)
  , pop(0, B), pop(0, A)
  , ( {A=B} -> push(true) ; push(false) )
@-> =(eq_)
  , pop(0, B), get(0, A)
  , ( {A=B} -> push(true) ; push(false) )
@-> =(neq)
  , pop(0, B), pop(0, A)
  , ( {A=B} -> push(false) ; push(true) )
@-> =(neq_)
  , pop(0, B), get(0, A)
  , ( {A=B} -> push(false) ; push(true) )

  "foo" -- push string on stack
  123.4 -- push number on stack
  {X}   -- push arbitrary term on stack

@-> =({X}) push(X)
@-> =(X) ?{string(X);number(X)}, push(X)
@-> =(X) ?{member(X,[null, true, false])}, push(X)

  rm   -- remove top of stack
  $N   -- move N-th stack element on top
  dup(N) -- copy N-th stack element on top

@-> =(rm(N)) pop(N,_)
@-> =(rm) pop(0,_)
@-> =($N) ?{number(N)}, pop(N,X), push(X)
@-> =(dup(N)) ?{number(N)}, get(N,X), push(X)
@-> =(dup)  verb_(dup(0))

  log  -- print top element from stack to the log

@-> =(log)
  , pop(0, X), {format('LOG: ~q\n', [X])}
@-> =(log_)
  , get(0, X), {format('LOG: ~q\n', [X])}


  $P  -- put on stack value at path P in the current object

@ -> =(Ctx, $A:B) exec1(Ctx, $(A:B))
@ -> =(Ctx, $P)  expr(Ctx, E)

@ -> =([St|_],#(N))
  , {I is N, nth0(I,St,V)}
  , push(V)


@== is_false +X
@-- =(false) ?
@-- =(null) ?


Stack Manipulations
--------------------------------------------

@== push +X +St0 ?St
@-- =(X, St, [X|St])

@== get +N -X +St0 ?St
@-- =(N, X, St, St)
  , nth0(N, St, X) ! shallow_stack(get(N))

@== pop +N -X +St0 ?St
@-- =(N,_,[],_) fail ! shallow_stack(pop(N))
@-- =(0, X, [X|St], St)
@-- =(N, X, [Y|St0], [Y|St])
  , N1 is N-1, pop(N1, X, St0, St)


--------------------------------------------

% :- initialization run_tests(eval).