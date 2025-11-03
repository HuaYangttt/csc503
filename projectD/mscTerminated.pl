:- consult('currentCourses.pl').
:- consult('mainUtils.pl').

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