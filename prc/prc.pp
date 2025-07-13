:- module(prc,[
  explore/1, print_pr/1, print_trace/1
]).
:- use_module(library(dcg/high_order)).

Process terms:

- A;P   -- action A followed by process P
- A/{G};P -- action with guard
- {G};P   -- guard G on state and then behave as P
- [P1,P2,..,Pn]  -- parallel processes with internal communications only
- [..]/CC    -- parallel process with channels from CC opened
- P1 | P2 | ... | Pn   -- alternatives

A process term  P  can be also augmented with process information  I  -- term  I:P  represents such augmented process.

Anything else is treated as process definition given by  usr:prc_def/2.

Action is one of:

- C << Data  -- send data to channel C
- C >> Data  -- recievie data from C

TODO:
- trc(T):P  -- add  T  to the trace and continue as  P.

:- multifile user:prc_def/2.

@== explore +Pr

Explore process interactively.

@-- explore(Pr,_)


@== get_move -N   @- get_move(_,N)
@== get_move +TF -N

Select move or give other disposition while interactively exploring process. Entering a number selects move with that number, entering anything else gives other dispositions as coded below.

TODO: `other` choice.

@--
  , read_line_to_string(user_input, S)
  , ( 
      S = "" -> N = other ;
      S = "x" -> N = done ;
      S = "q" -> N = done ;
      S = "f" -> N = giveup ;
      S = "a" -> N = abort ;
      S = "t" -> !, get_move(true,N) ;
      !, number_string(N0,S), ( TF==true -> N = trace:N0 ; N=N0 )
    )

@== exp_step +Tr
@--
  , print_trace(Tr), nl
  , format('??? continue ("q" to quit)> ')
  , read_line_to_string(user_input, S), nl
  , S = "q"


@+> user:print +X
@-> =(prc(_:P))  print(prc(P))
@-> =(prc(P/_))  print(prc(P))
@-> =(prc([P|PP]))
  , {maplist([X,prc(X)]>>true, [P|PP], PP1)}
  , sequence(print,['|'],PP1)
@-> =(prc(A;P))    [pact(A),'.',prc(P)]
@-> =(prc(P|Q))    [prc(P),' + ',prc(Q)]
@-> =(prc(P))      ['~q' - [P]]

@+> user:print +X
@-> =(pact(A/G))  [pact(A), {}, '/~q' - [G]]
@-> =(pact(P<<D)) ['~w'-[P], {}, io_dir(snd), {}, '~q'-[D]]
@-> =(pact(P>>D)) ['~w'-[P], {}, io_dir(rcv), {},'~q'-[D]]


@== print_pr +Pr  @- print_pr([], [], Pr)
@== print_pr +F +I +Pr 
@-- =(F, I, _:P)  print_pr(F, I, P)
@-- =(F, I, P/_)  print_pr(F, I, P)
@-- =(F, I, [P])  print_pr(F, I, P)
@-- =(F, I, [P|PP])
  , append(F, [{}, '| '], F1)
  , maplist([A,B]>>(A='| ' -> B=': ' ; B=A), I, I0)
  , append(I0, [{}, '| '], I1)
  , print_pr(F1, I1, P)
  , maplist({I1}/[X]>>print_pr(I1,I1,X), PP)
@-- =(F, _, P) 
  , append(F, [{}, prc(P)], L), print_line(L)



--------

Action terms:

  - I:io:P:D:X  -- send (when  X=snd) or receive (when  X=rcv) data  D  on port  P.

  - I:sync:P:D  -- inernal communication with data  D  on port  P.
  - I:trc:T     -- add term T to the trace.

In each case dict I gives additional information about the action. 

@+> user:print +X
@-> =(act(I:io:P:D:X))
  , ['~w' - [P], {}, io_dir(X), '~q' - [D]], io_guard(I)
@-> =(act(_:sync:P:D))   ['~w - ~q' - [P,D]]
@-> =(act(M))            ['~q' - [M]]

@+> user:print +X
@-> =(io_dir(rcv))  ['?']
@-> =(io_dir(snd))  ['!']

@=> io_guard +I
@-> ? {get_dict(guard,I,G)}, ['/{~q}' - [G]]
@-> (_)


@== next_act +Pr0 -A -Pr

Term  A  describes possible action of process  Pr0  and process  Pr  is the continuation after action  A.

TODO: {G}:Pr

@-- =(_:done,_,_) fail
@-- =(_:[],_,_) fail

@-- =(I:P0, A, P)
  ? get_dict(more,I,_)
  , reduce(P0, P1), next_act(P1, A, P) ?

  Communication step is described by term  C:X  where  C  is the channel and  X = Y/G:D  describes direction  Y  (snd/rcv), data  D  sent or received and guard  G  on the data.

@-- =(_:(P<<D/{G};Cont), A, Cont)
  , A = ai{guard:G}:io:P:D:snd
@-- =(_:(P>>D/{G};Cont), A, Cont)
  , A = ai{guard:G}:io:P:D:rcv
@-- =(_:(P<<D;Cont), A, Cont)
  , A = ai{}:io:P:D:snd
@-- =(_:(P>>D;Cont), A, Cont)
  , A = ai{}:io:P:D:rcv

  Choice

@-- =(_:(P0 | _), A, P)
  ? next_act(P0, A, P) ?
@-- =(_:(_ | P0), A, P)
  , next_act(P0, A, P) ?

  Parallel composition

@-- =(_:[P0|PP0], A, P)
  , pp_next([], [P0|PP0], A, P) ?
@-- =(_:[P0|PP0]/EE, A, P) 
  , pp_next(EE, [P0|PP0], A, P) ?

@-- =(I:P, _, _) fail ! next_act(I:P)
@-- =(P0, A, P)
  , reduce(P0, P1), next_act(P1, A, P) ?

@ -- =(P0,_,_) fail ! next_step(P0) 


@== pp_next +EE +PP0 -A -P

Term  A  describes next action of parallel process composition  PP0  with external communications happening only on chanells from  EE  with  P  being the continuation after action  A. 

@-- =(_, [], _, _)  fail

@-- =(Ext, [P0], A, P)
  , next_act(P0, A, P1)
  ? ext_check(Ext, A)
  ? ( Ext = [-] -> P = P1 ; P = [P1]/Ext ) ?

@-- =(Ext, [Pa,Pb|PP0], I:sync:A, PP/Ext)
  ? subset([Pa,Pb|PP0],[Vp=P,Vq=Q],PP)
  ? pp_sync(P, Q, I:A, P1, Q1)
  ? Vp = P1 ? Vq = Q1 ?

@-- =(EE, PP0, I:X:A, PP/EE)
  ? subset(PP0,[V=P],PP)
  ? next_act(P, I:X:A, P1) 
  ? ( X = io -> ext_check(EE, A) ; true )
  ? V=P1 ?


@== pp_sync +P +Q +IA -P1 -Q1
@-- =(Ip:P, Iq:Q, ai{}:X:D, P1, Q1)
  , (PP = Ip.get(io,[]))
  , (QP = Iq.get(io,[]))
  , ext_intersect(PP, QP, XX)
  ? member(X, XX)
  ? next_act(Ip:P, _:io:X:D:A, P1)
  ? io_dir(A, B)
  ? next_act(Iq:Q, _:io:X:D:B, Q1)
  ? ground(D)
  % TODO ? once((ground(D), call(GP), call(GQ)))


@== io_dir ?A ?B
@-- =(snd,rcv) ?
@-- =(rcv,snd) ?


:- begin_tests(prc_next).


:- end_tests(prc_next).



# External port specifications

Such specification is a list of items:

- atom A -- external i/o allowed on port A
- term -A -- external i/o forbidden on port A

Note: If there are any  -X  terms in a specification then all ports that are not hidden are external. Otherwise (no  -X  terms in EE) only the explicitly specified ports are external.


@== ext_spec ?EE ?A ?B

Port sets  A  and  B  are such that  EE = A \cup ~B.

Note: Only for non-empty spec EE.

@-- ?var(EE), ext_spec_(EE, A, B)
@-- =([], [], {})
@-- =([-|_], [], [])

@-- =([-X|EE], A, B)
  , (
      EE=[] -> A=[], B=[X] ;
      ext_spec(EE, A, B0), pp_union([X], B0, B)
    )

@-- =([X|EE], [X|A], B)   ( EE=[] -> A=[], B={} ; ext_spec(EE, A, B) )


@== ext_spec_ -EE +A +B
@-- =([-], [], [])
@-- =([], [], {})
@-- =([X|EE], [X|A], B)   ( A=[], B={} -> EE=[] ; ext_spec_(EE,A,B) )
@-- =([-X|EE], [], [X|B]) ( B=[] -> EE=[] ; ext_spec_(EE,[],B) )


@== ext_check +EE +A

Check if external i/o action  A  is allowed according to spec EE.

@-- =([-], _)
@-- =(EE, P:_)  ext_intersect([P], EE, [P])
@-- =(_,_) fail ! ext_check

  Note any other actions that do not send anything to ports are allowed.
  

@== ext_union +E0 +E1 -EE

@-- =([], EE, EE)
@-- =(EE, [], EE)
@-- =([-|_], _, [-])
@-- =(_, [-|_], [-])

  (A0 \cup ~B0) \cup (A1 \cup ~B1)
  = (A0 \cup A1) \cup (~B0 \cup ~B1)
  = (A0 \cup A1) \cap ~(B0 \cap B1)

@--
  , ext_spec(E0, A0, B0), ext_spec(E1, A1, B1)
  , pp_union(A0, A1, A)
  , pp_intersection(B0, B1, B)
  , ext_spec(EE, A, B)

@== ext_union +EEE -EE
@-- =([], [])
@-- =([E], E)
@-- =([E0,E1|EEE], EE)
  , ext_union(E0, E1, E), ext_union([E|EEE], EE)


@== ext_intersect +E0 +E1 -EE

Note: We assume E0 and E1 are normalized.

@-- =([], _, [])
@-- =(_, [], [])

@-- =([-|_], EE, EE)
@-- =(EE, [-|_], EE)

@-- =([-P], E1, EE)
  , ext_spec(E1, A1, B1)
  , pp_sub(A1, [P], A)
  , pp_union([P], B1, B)
  , ext_spec(EE, A, B)

@-- =([P], E1, EE)
  , ext_spec(E1, A, B)
  , (
      member(P,A) -> EE=[P] ; 
      B = {} -> EE=[] ; 
      member(P,B) -> EE=[] ; EE=[P]
    )

 (A0 \cup ~B0) \cap (A1 \cup ~B1)
 = ((A0 \cup ~B0) \cap A1) \cup ((A0 \cup ~B0) \cap ~B1)
 = (A0 \cap A1) \cup (A1 \cap ~B0) \cup (A0 \cap ~B1) \cup (~B0 \cap ~B1)
 = (A0 \cap A1) \cup (A1 - B0) \cup (A0 - B1) \cup ~(B0 \cup B1)

Note:  A \cap ~B = A - B

@--
  , ext_spec(E0, A0, B0), ext_spec(E1, A1, B1)
  , pp_intersection(A0, A1, C0)
  , pp_sub(A1, B0, C1)
  , pp_sub(A0, B1, C2)
  , pp_union(B0, B1, D)
  , pp_union(C1, C2, C3), pp_union(C0, C3, C)
  , ext_spec(EE, C, D)

@== ext_intersect +EEE -EE
@-- =([], [-])
@-- =([E], E)
@-- =([E0,E1|EEE], EE)
  , ext_intersect(E0, E1, E), ext_intersect([E|EEE], EE)



@== pp_union +A +B -X
@-- =({}, _, {})
@-- =(_, {}, {})
@-- ord_union(A,B,X)

@== pp_intersection +A +B -X
@-- =({}, B, B)
@-- =(A, {}, A)
@-- ord_intersection(A, B, X)

@== pp_sub +A +B -X
@-- =({}, _, _) fail ! pp_sub
@-- =(_, {}, [])
@-- ord_subtract(A, B, X)


# Eliminating dead sub-processes

@== reduce_full +P0 -IP
@-- reduce(P0,I:P1)
  , ( get_dict(more,I,_) -> reduce_full(I:P1,IP) ; IP = I:P1 )

@== reduce +P0 -IP  @- reduce([-],P0,IP)
@== reduce +Ext +P0 -IP

Term  IP  describes reduced process augmented with static analysis information which is equivalent to process  P0  under assumption that all external ports are given by  Ext.

Note: The reduced process has external port specification in place if needed.

@-- =(Ext, I0:P0, I:P)
  %, trace
  , ( EE = I0.get(ext,[-]) )
  , ext_intersect(Ext, EE, EE1)
  , reduce(EE1, P0, I1:P)
  %, ( _{ ext:XX } :< I1, member(-fst_,XX) -> trace ; true )
  , refine_info(I0,I1,I2)
  , ( get_dict(more,I1,_) -> I=I2 ; del_dict(more, I2, _, I) ; I=I2 )

@-- =(_, done, pi{ext: [], io: []}:done)

Asumming i/o can happen only on ports in  Ext  analyze and reduce process  P0. Term  IP = done  if  P0  is blocked.  Otherwise  IP = I:P  where  P  is a reduced process equivalent to  P0  and dict  I  contains information about the process.

@-- =(Ext, X<<D; P0, IP)
  , ( 
      ext_intersect([X], Ext, []) -> IP = pi{}:done ;
      IP = pi{io: [X]}: (X<<D; P0)
    )
@-- =(Ext, X<<D/G; P0, IP)
  , reduce(Ext, X<<D; P0, I:P)
  , ( 
      P=done -> IP = pi{}:done ;
      P=(A;P0), IP = I: (A/G; P0)
    )

@-- =(Ext, X>>D; P0, IP)
  , ( 
      ext_intersect([X], Ext, []) -> IP = pi{}:done ;
      IP = pi{io: [X]}: (X>>D; P0)
    )
@-- =(Ext, X>>D/G; P0, IP)
  , reduce(Ext, X>>D; P0, I:P)
  , ( 
      P=done -> IP = pi{}:done ;
      P=(A;P0), IP = I: (A/G; P0)
    )


@-- =(Ext, Q0 | R0, I:P)
  , reduce(Ext, Q0, IQ:Q)
  , reduce(Ext, R0, IR:R)
  , (
      Q = done -> I:P = IR:R ; 
      R = done -> I:P = IQ:Q ;
      P = (Q|R), combine_info([IQ,IR],I)
    )

DOCME...

@-- =(Ext, [P0|PP0]/[-], IP)  
  , reduce_pp(Ext, [P0|PP0], IP0)
  , reduce_pp1(Ext, IP0, IP)

@-- =(Ext, PP/EE, IP)
  , ext_intersect(Ext, EE, EE1)
  , reduce(EE1, PP/[-], IP)

@-- =(_, [P|PP], IP)  reduce([], [P|PP]/[-], IP)


@-- =(Ext, P, pi{ext: Ext, more: {}}: Def)  ? prc_def(P, Def)
@-- =(Ext, P, pi{ext: Ext, io:Ext, nonio:{}}: P)


@== reduce_pp1 +Ext +IP0 -IP
@-- =(_, _:done, IP)  reduce(done, IP)
@-- =(Ext, I0:P0, I:P)
  , (
    is_list(P0) -> P = P0/Ext ;
    Ext = [-] -> P = P0 ; P = [P0]/Ext 
  )
  , reduce_pp2(Ext, I0, I)

@== reduce_pp2 +Ext +I0 -I
@-- =(_, I, I) ? _{more:_} :< I.
@--
  , dict_pairs(I0,_, PI0)
  , ( 
    select(io-XX0, PI0, PI0_) ->
      ext_intersect(Ext,XX0,XX), PI1=[io-XX|PI0_]; PI1 = PI0
  )
  , ( 
    select(ext-EE0, PI1, PI1_) ->
      ext_intersect(Ext,EE0,EE), PI2=[ext-EE|PI1_]; PI2 = PI1
  )
  , dict_pairs(I, pi, PI2)


@== reduce_pp +Ext +PP -IP

Term  IP=I:P  describes reduced process  P  equivalent to the parallel composition of processes in  PP  with external ports limited to  Ext. If  P  is a parallel composition then sub-processes are augmented.

@-- =(_, [], pi{}:done)

@-- =(Ext, [PP/EE], IP)
  , ext_intersect(Ext, EE, Ext1)
  , reduce_pp(Ext1, PP, IP)

@-- =(Ext, [P0], IP)
  , reduce(Ext, P0, IP)

Given more than one process in parallel composition we first reduce each of them and then try to remove blocked ones. If some are removed we continue with  reduce_pp/3  on the remaining ones. Otherwise the list of processes is unchanged but we combine information obtained when reducing individual processes.

FIXME: If one of the sub-prc can be further reduced then stop other processing but don't put `more` in the process info?

@--
  , maplist(reduce, PP, IPP)
  , maplist([I_:P,I_]>>true, IPP, II)
  , pp_info(II, I)
  , ( get_dict(more, I, _) -> 
        YN = no, IQQ = IPP ; 
        pp_rm_blocked(YN, Ext, IPP, IQQ)
    )
  , (
      YN == yes -> 
        !, maplist([I:P,P]>>true, IQQ, QQ),
        reduce_pp(Ext, QQ, IP) ; 
      IP = I:IQQ
    )


@== pp_rm_blocked ?YN +Ext +IPP -IQQ
@--
  ? select(IP, IPP, IQQ0)
  ? pp_blocked(Ext, IP, IQQ0)
  , YN = yes, pp_rm_blocked(_, Ext, IQQ0, IQQ)
@-- =(_, _, IPP, IPP)  


@== pp_blocked +Ext +IP +IPP

Process of  IP  can not perform any action when run in parrallel with processes of  IPP  with external ports  Ext.

@-- =(_, I:_, _) ? get_dict(nonio, I, _), fail

  Note: Process that can do non-i/o action is never blocked.

Here check proces that can do only i/o actions. If no such actions are possible it is blocked. Otherwise invoke  pp_blocked_/3  to check if sync with other processes is blocked.

@-- =(Ext, I:_, IPP)
  , (XX=(I.get(io,[])))
  , ( XX = [] ; pp_blocked_(Ext, XX, IPP) )


@== pp_blocked_ +Ext +XX +IPP

Process with next action being i/o on one of the ports from  XX  can not perform any action when running in parallel with processes of  IPP  with externl ports  Ext.

@-- (IPP) ? ext_intersect(Ext,XX,Y) ? Y\=[], fail

  Note: The process can do external i/o.

@-- (Ext)
  ? member(I:_, IPP)
  ? (YY = I.get(ext,[-]))
  ? ext_intersect(XX, YY, Z) ? Z\=[], fail

  Note: The process can sync with other process.

@-- (_)


# Managing process information

Process information is a dictionary with the following keys:

- io     -- list of ports on which process can do i/o in the next step
- nonio  -- if present then process can do non-i/o action in the next step
- ext    -- list of ports such that any i/o the process can ever do is on a port from that list.
- more   -- present if there is a possibility of reducing the process further (it can turn out that the process is not reducible after all)

TODO: rename `more` -> `reduce`.


@== combine_info +II -I  @- combine_info(yes, II, I)
@== combine_info +More +II -I

Combine information about several sub-processes into information about a combined process in which any of the sub-processes can execute: either a parallel composition or nondet. choice. Combined information does not include  more  keyword unless  More  is  yes  in which case it contains it if any of the sub-processes has it.

@--
  , findall(XX, (member(I1_, II), _{io:XX} :< I1_), IOO0)
  , findall(YY, (member(I2_, II), _{ext:YY} :< I2_), EEE0)
  , ( (member(I3_, II), _{nonio:_} :< I3_) -> NIO = [nonio-{}] ; NIO = [] )
  , (
    (More = yes, member(I4_,II), _{more:_} :< I4_)
    -> M=[more-{}] ; M=[]
  )
  , ( EEE0=[] -> EE=[], EE0=[-] ; ext_union(EEE0, EE0), EE=[ext-EE0] )
  , maplist({EE0}/[IOa,IOb]>>(ext_intersect(EE0,IOa,IOb)), IOO0, IOO1)
  , ( IOO0=[] -> IO=[] ; ext_union(IOO1, IO0), IO=[io-IO0])
  , append([M,NIO,IO,EE], KV)
  , dict_pairs(I, pi, KV)


@== refine_info +I0 +I1 -I

Terms  I0  and  I1  give information about the same process and  I  is a combined information taking into account both of these.

@--
  , dict_pairs(I1, _, PI1)
  , (
    get_dict(ext, I0, E0) ->
      (
        select(ext-E1, PI1, PI1_) -> 
          ext_intersect(E0, E1, EE),
          PI2 = [ext-EE | PI1_]
        ; PI2 = [ext-E0 | PI1]
      ),
      (
        select(io-X0, PI2, PI2_) ->
          ext_intersect(E0, X0, XX),
          PI = [io-XX | PI2_]
        ; PI = PI2
      )
    ; PI = PI1
  )
  , dict_pairs(I,pi,PI)


@== pp_info +II -I
@--
  , combine_info(yes, II, I0)
  , (II = [_,_|_] -> I = (I0.put(nonio,{})) ; I = I0)


:- begin_tests(prc_reduce).

@+= test +T
@-- =(e5)
  , reduce_full(pi{ext:[]}:(p<<{};foo), _:P)
  , assertion(P == done)
@-- =(e4)
  , reduce_full([p<<{};done]/[-q], I:_)
  , assertion(_{io:[p], ext:[-q]} :< I)
@-- =(e3)
  , reduce_full([p<<{};done]/[q], _:P)
  , assertion(P == done)
@-- =(e2)
  , reduce_full([p<<{};done], I:_)
  , assertion(_{io:[], ext:[]} :< I)
@-- =(e1)
  , reduce_full(p<<{};done, I:_)
  , assertion(_{io:[p]} :< I)

@+= test +T
@-- =(d)
  , P0 = [p<<{};done,p>>{};done]/[-p]
  , reduce([p<<{};done, P0]/[-p], _:P)
  , reduce(P0, _:P1)
  , assertion(P == P1)

@+= test +T + Opt
@-- =(c2, true(P = _/[p,q]))
  , reduce([foo]/[p,q], I:P)
  , assertion([p,q] == I.ext)
@-- =(c1, true(P == done))
  , reduce([], [done], _:P)


@== block_t +Ext ?PP ?YN
@-- =([-q], q<<{};done, yes)?
@-- =([p], q<<{};done, yes)?
@-- =([-p], q<<{};done, no)?

This example shows that it is not enought to look at the initial acion of each process. The first process has no sync for the initial action  q<<{}  but it is not blocked because after the two other processes synchronize the 2nd one will be able to receive data at  q.

@-- =([], [q<<{};done, p>>{};q>>{};done, p<<{};done], no)

In this example both sub-prc are blocked. First one can never sync on port  q  because the only other sub-prc has  q  excluded from the external ports. The 2nd one is blocked because it can not do external i/o on port  q  and it has no other process to sync with.

@-- =([], [q<<{};done, [q>>{};done]/[-q]], yes)


@+= test +T +Opt
@-- =(b, forall(block_t(Ext,P0,yes)))
  , reduce(Ext, (P0 | foo), _: P1), assertion(P1 == foo)
  , reduce(Ext, (foo | P0), _: P2), assertion(P2 == foo)
  , reduce(Ext, (P0 | P0), _: P3), assertion(P3 == done)

@+= test +T +Opt
@-- =(a3, forall(block_t(Ext, P, yes)))
  , reduce(Ext, P, _:P1)
  , assertion(P1 == done)
@-- =(a2, forall(block_t(Ext, P, YN)))
  , reduce([-], [P]/Ext, P1)
  , assertion(P1 = _:done ; YN = no)
@-- =(a1, forall(block_t(Ext, P, YN)))
  , reduce(Ext, P, P1)
  , assertion(P1 = _:done ; YN = no)

:- end_tests(prc_reduce).


/== EX:
This process is blocked (assuming external i/o on port  p  is not allowed):
------
 | p << {} : done
 | Pr/[-p]
------

Because first process can only do i/o on port  p  and second process can not do i/o on that port. We have  io_ports(A,[p])  and  io_ports(B,[-p])  and the intersection of  [p]  and  [-p]  is empty. The union of these port sets is  [-]

Similar example:
------
  | A/[p]
  | B/[q]
------
\====


@== subset +XX0 -Sel -XX
@-- =([A|XX],[Va=A],[Va|XX]) ?
@-- =([A,X|XX],[Va=A,Vb=B],[Va|YY])
  ? subset([X|XX],[Vb=B],YY) ?
@-- =([X|XX],AB,[X|YY]) subset(XX,AB,YY) ?


---------------

:- multifile user:explore_str/2.

@== explore +Pr0 -Pr
@--
  , retractall(exp_trace)
  , Tr = [1]:[Pr0|U]-U
  , setup_call_cleanup(prompt(Old,''),(
      explore_(Pr0, Tr)
    ), prompt(_,Old))
  , trace_get(Tr, Pr)


@== explore_ +Pr ?Tr
@--
  , reduce(Pr, I:Pr1)
  , trace_extend(Tr, Pr1, Tr1)
  , ( 
      exp_step(Tr1) ;
      get_dict(more,I,_) -> explore_(Pr1, Tr1) ;
      explore0(I, Pr1, Tr1)
    )

@== explore0 +I +Pr + Tr

Explore process  Pr  that could not be reduced any more. Term  I gives information about the process from reducion attempt.

@-- (I)
  , findall(A-Pr1, next_act(Pr,A,Pr1), MM0)
  , maplist([A-_,A]>>true, MM0, AA)
  , ( 
      explore_str(AA, AA1) -> include({AA1}/[X-_]>>member(X,AA1),MM0,MM) ;
      MM = MM0
    )
  , explore1(MM, Tr)


@== explore1 +MM +Tr

Explore possible process moves -- MM  is a list of terms  M:Pr  where  M  is a move of the current process and  Pr  is the continuation after that move.

@-- =([], Tr)
  , nl, print_trace(Tr), nl
  , print_line('== no more moves ==')
  , nl
  , continue(Tr)

@-- =([M], Tr)
  , explore_move(M, Tr)
@--
  , trace_fork(Tr, Tr1)
  , get_move(Tr, MM, M)
  , explore_move(M, Tr1)


@== continue +Tr

Continue after process has terminated

@-- (_)
@ --
  , trace_bt(Tr, Pr, Tr1)
  , explore_(Pr, Tr1)


@== explore_move +X +Tr

Explore after selection  X  which can be a move  M:Pr  to pursue or one of the other dispositions.

@-- =(done,_)
@-- =(giveup, _) fail
@-- =(abort, _) throw(abort)
@-- =(trace:M, Tr) trace, explore_move(M,Tr)
@-- =({}-Pr, Tr)
  , trace_extend(Tr,Pr,Tr1)
  , explore_(Pr,Tr1)
@-- =(M-Pr, Tr)
  , trace_extend(Tr,{M},Tr1)
  , trace_extend(Tr1,Pr,Tr2)
  , explore_(Pr,Tr2).


@== get_move +Tr +MM -M
@-- =(NN:Tr, MM, M)
  , print_trace(NN:Tr)
  % , NN = [N0|_], print_line(['??? #~d' - [N0]])
  , print_moves(MM), nl
  , NN = [N0|_], format('??? #~d ("q" to quit)> ', [N0]), flush
  , get_move(N), nl 
  , ( 
      N = trace:N0 -> nth1(N0,MM,M0), M=trace:M0 ;
      number(N) -> nth1(N,MM,M) ;
      M = N
    )

@== print_moves +MM
@-- foreach(nth1(N,MM,M), print_mv(N,M))

@== print_mv +N +M
@-- =(N,M-Pr)
  , nl
  , maplist(print_line, [
      [ '~d:' - [N], act(M), '-->' ],
      [ prc(Pr) ]
    ])

------------

# Traces

Trace is described by term  NN:T-U  where  T-U  is a difference list describing process trajectory so far. The trajectory is a list of process terms separated by {A} terms where  A  is an action performed by preceeding process leading to the process that follows {A} in the trace.

Term NN is a list of branch positions along the trace (1-based, more recent first). The first element of NN is the current position in the trace (i.e, its length).


@== trace_extend +Tr0 +X -Tr
@-- =([N|NN]:T-U, X, [N1|NN]:T-U1)
  , U=[X|U1], N1 is N+1

@== trace_fork +Tr0 -Tr
@-- =([N|NN]:Tr, [N,N|NN]:Tr) 

@== trace_bt +Tr0 -Pr -Tr
@-- =([_,N|NN]:Tr0-_, Pr, [N|NN]:Tr)
  , trace_get(N,Tr0,Pr,Tr)

@== trace_get +N +Tr0 -Pr -Tr
@-- =(1, [{A}|Tr0] , Pr, [{A}|T]-U)
  , trace_get(0, Tr0, Pr, T-U)
@-- =(1, [Pr|_], Pr, [Pr|U]-U)
@-- =(N, [X|Tr0], Pr, [X|T]-U)
  , N1 is N-1
  , trace_get(N1, Tr0, Pr, T-U)


@== trace_get +Tr -Pr

Get the last process in the trace

@-- =(_:Tr-_, Pr)
  , reverse(Tr,[Pr|_])


@== print_trace +Tr
@-- =([_,N|_]:T-_) print_trace1(N, T)
@-- =(_:T-_)  print_trace1(1, T)

@== print_trace1 +N +X
@-- =(1, [X|XX]) ? var(XX), print_trace1(1, [X])
@-- =(_, [_|XX]) ? var(XX)
@-- =(1, [{M}])
  , nl, maplist(print_line, [':',[':', act(M)],':']), nl
@-- =(1, [Pr])
  , print_pr(['* '], ['  '], Pr)
@-- =(1, [X|XX])
  , print_trace1(1,[X]), print_trace1(1,XX)
@-- =(N, [_|XX])
  , N1 is N-1, print_trace1(N1,XX)

---------------

@== prc_def +P -Def  
@-- ? user:prc_def(P, Def)


----------------------------

# Forth processes

@+= prc_def +P -Def  
@-- =(f:CC, Pr)  prc_def(f([]):CC, Pr)
@-- =(f(St):FF, [fst_, f_(St,FF)]/[-fst_])


@-- =(f_(St,FF), Pr)
  , forth:exec(St:FF,St1,I) ! forth_exec(St,FF)
  , forth_prc(St1,I,Pr) ! forth_prc(St1,I)


@=> forth:verb +V -I

Forth verbs used in process definitions. These generate interrupts that are used to construct appropriate process.

@-> =(async:FF, async(FF)-[])
@-> =(await:CC, await(CC)-[])
@-> =(rcv(C), rcv(C)-[])
@-> =(snd(C), snd(C)-[])
@-> =(!(P), set(P)-[])
@-> =($(P), get(P)-[])
@-> =(idle, idle-[])
@-> =(cont(Pr), cont(Pr)-[])


@== forth_prc +St +I -Pr

Continuation after Forth code interuption  I  at stack  St.

@-- =(_, {}, done)
@-- =(_, done-_, done)

Set or get value at a given path in the object.

@-- =(St, set(P)-FF, fst_ << set(P,V) ; Pr )
  , St = [V|St1]  ! empty_stack(set)
  , prc_def(f_(St1,FF), Pr)

@-- =(St, get(P)-FF, fst_ << get(P) ; fst_ >> X ; f_get(P,X,St-FF))

Send or receive data on port.

@-- =(St, snd(C)-C1, [ C << X ; prm_ << {} ; done, Pr]/[-prm_]) 
  , St = [X|St1] ! empty_stack(snd)
  , prc_def(f_([prm{}|St1],C1), Pr)

@-- =(St, rcv(C)-C1, [ C >> X ; prm_ << {X} ; done, Pr ]/[-prm_])
  , prc_def(f_([prm{}|St],C1), Pr)

  Note: snd(C) and rcv(C) are asynchronous commands which produce promises that can be used by the continuation.

Execute forth code asynchronously.

TODO: promise should return top stack value after exe

@-- =(St, async(FF)-CC, [A,B]/[-])
  , prc_def(f_(St,FF), A)
  , prc_def(f_([prm{}|St],CC), B)

Continuation after await request. Forth code  DD  should produce a promise (or just plain value) which will be used by  @await  interupt to arrange waiting for promise's value.

@-- =(St, await(DD)-C1, Pr)
  , prc_def(f_(St,[DD,@(await)|C1]), Pr)

After  @await  interupt, if we have promise object on top of the stack, we wait for that promise result that should arrive on channel  prm_, the continuation  f_resolve()  will check if promise produced a value and if this is the case it will push it on stack before continuing.

@-- =(St, await-C1, prm_ >> P ; f_resolve(P,St1,C1))
  ? St = [prm{}|St1]

If no promise was produced then we simply use current stack top as outcome.

@-- =(St, await-C1, Pr)
  , prc_def(f_(St,C1), Pr)

@-- =(St, idle-FF, idle_ >> _ ; f_(St,FF))

@-- =(_, cont(Pr)-_, Pr)

@-- =(_,I,_)    throw(forth_cont(I))


@+= prc_def +P -Def  

Forth code continuation after promise await. The first argument is {X} if promise produces value X and {} otherwise.

@-- =(f_resolve({},St,CC), Pr)
  , prc_def(f_(St,CC), Pr)

@-- =(f_resolve({X},St,CC), Pr)
  , prc_def(f_([X|St],CC), Pr)

@-- =(f_get(P,at(P,V),St-FF), Pr)
  , prc_def(f_([V|St],FF), Pr)
@-- =(f_get(P,undef(P),St-FF), Pr)
  , prc_def(f_([null|St],FF), Pr)
@-- =(f_get(P,X,_), _)
  , fail ! f_get(P,X)

@+= user:prc_def +P -D
@-- =(idle, idle_ << {} ; idle)


@+= user:prc_def +D -Pr

Forth object state

@-- =(fst_, Pr) prc_def(fst_(f{}), Pr)
@-- =(fst_(St), (
  fst_ >> set(P,V) ; fst_set(St,P,V) |
  fst_ >> get(P) ; fst_get(St,P)
))

@-- =(fst_set(St,P,V), fst_(St1))  set_path(St,P,V,St1)
@-- =(fst_get(St,P), fst_ << at(P,V) ; fst_(St))
  ? get_path(St,P,V)
@-- =(fst_get(St,P), fst_ << undef(P) ; fst_(St))


@== set_path +St0 +P +V -St
@-- =(St0, A:P, V, St)
  , get_dict(A, St0, St1) ! bad_path(A:P, St0)
  , set_path(St1, P, V, St2)
  , set_path(St0, A, St2, St)
@-- =(St0, A, V, St)
  , put_dict(A, St0, V, St)

@== get_path +St +P -V
@-- =(St, A:P, V)
  , get_dict(A, St, St1)
  , get_path(St1, P, V)
@-- =(St, A, V)
  , get_dict(A, St, V)


# Debug trace

@+= prc_def +P -D
@-- =(dbg, dbg([])) % [idle, dbg([])]/[-])
@-- =(dbg(Tr), tr_ >> X ; dbg([X|Tr]))

@+> forth:verb +V -I
@-> =(trace, I) forth([await: [snd(tr_)]], I)
