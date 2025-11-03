:- consult('currentCourses.pl').
:- consult('mainUtils.pl').

% Termination criteria for PhD:
% - Failing to pass the Oral Preliminary exam within 6 years from admission.
% - Failing to complete all requirements within 10 years from admission.
% - Accumulating 18 or more graduate credit hours with GPA below 3.0.
% Returns all reasons why a PhD student must be terminated
hasToBeTerminated(StudentID, phd) :-
    registrationSemester(StudentID, Program, _SemStart, YearStart, IsFirst),
    Program == 'phd', IsFirst == 'yes', !,
    findall(Reason,
        (
            (
                YearStart =< 2019,
                \+ (phdOralExamTaken(StudentID, _Sem, _Yr, Outcome), Outcome == 'pass'),
                Reason = 'Failed to pass Oral Preliminary Exam within 6 years'
            );
            % check continuous enrollment
            ( 
                \+ continuous_enrollment_from_start(StudentID),
                Reason = 'Violates continuous enrollment (missing some Fall/Spring record)'
            );
            (
                YearStart =< 2015,
                Reason = 'Exceeded 10-year time limit for PhD completion'
            );
            (
                findall(U, (hasTakenCourse(StudentID, _CID, _Sect, U, Grade), number(Grade)), UnitsList),
                sum_list(UnitsList, SumUnits), SumUnits >= 18,
                list_taken_courses(TakenList, StudentID), TakenList \= [],
                student_gpa(StudentID, TakenList, GPA), GPA < 2.0,
                Reason = 'Accumulated >=18 credits with GPA below 3.0'
            )
        ),
        Reasons
    ),
    ( Reasons \= [] ->
        format('Student ~w must be terminated for the following reason(s):~n', [StudentID]),
        forall(member(R, Reasons), format('- ~w~n', [R]))
    ;
        format('Student ~w is in good standing (no termination criteria met).~n', [StudentID]), fail
    ).

