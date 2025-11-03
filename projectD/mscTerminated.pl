:- consult('currentCourses.pl').
:- consult('mainUtils.pl').

sem_index(spring, 0).
sem_index(fall,   1).

next_term(spring, Y, fall,  Y).
next_term(fall,   Y, spring, Y1) :- Y1 is Y + 1.

term_seq(Sem0, Y0, SemC, YC, Sem0, Y0).
term_seq(Sem0, Y0, SemC, YC, Sem,  Y) :-
    ( Sem0 = SemC, Y0 = YC -> fail
    ; next_term(Sem0, Y0, S1, Y1),
      ( Sem = S1, Y = Y1
      ; term_seq(S1, Y1, SemC, YC, Sem, Y)
      )
    ).

over_six_years_msc(StudentID) :-
    registrationSemester(StudentID, msc, Sem0, Y0, yes),
    current_semester(SemC, YC),
    Delta is YC - Y0,
    (  Delta > 6
    ;  Delta =:= 6,
       sem_index(SemC, IC),
       sem_index(Sem0, I0),
       IC > I0
    ).

continuous_enrollment_from_start(StudentID) :-
    registrationSemester(StudentID, msc, Sem0, Y0, yes),
    current_semester(SemC, YC),
    forall(
      term_seq(Sem0, Y0, SemC, YC, Sem, Y),
      ( registrationSemester(StudentID, msc, Sem, Y, _) -> true
      ; format('Student ~w missing enrollment in ~w ~w.~n', [StudentID, Sem, Y]),
        fail
      )
    ).

hasToBeTerminated(StudentID, msc) :-
    ( over_six_years_msc(StudentID) ->
        registrationSemester(StudentID, msc, _SemStart, YearStart, yes),
        format('Student ~w has exceeded the 6-year limit (started in ~w).~n', [StudentID, YearStart])
    ; \+ continuous_enrollment_from_start(StudentID) ->
        format('Student ~w violates continuous enrollment (missing semesters).~n', [StudentID])
    ; 
      format('Student ~w meets all MSC requirements and will NOT be terminated.~n', [StudentID]),
      fail
    ).