:- consult('currentCourses.pl').
:- consult('mainUtils.pl').

canGraduate(StudentID, msc) :-
    % 1. Must pass CSC600 (Graduate Orientation).
    ( hasTakenCourse(StudentID, 'csc600', _Sect, _Units, G600),
      is_passing_grade(G600)
    -> true
    ;  format('Fail: Must pass CSC600 (Graduate Orientation).~n', []), fail ),
    % 2. Core courses: at least 3 core courses total, with at least 1 theory core course.
    count_theory_course(StudentID, NumTheory), 
    count_systems_course(StudentID, NumSystems),
    TotalCore is NumTheory + NumSystems,
    ( NumTheory >= 1
    -> true
    ;  format('Fail: Need at least one Theory core course (current theory = ~w).~n', [NumTheory]), fail ),
    ( NumSystems >= 1
    -> true
    ;  format('Fail: Need at least one Systems core course (current systems = ~w).~n', [NumSystems]), fail ),
    ( TotalCore >= 3
    -> true
    ;  format('Fail: Need at least 3 total core courses (current total core = ~w).~n', [TotalCore]), fail ),
    % 3. CSC elective courses: at least 21 units of CSC 500- or 700-level coursework (max 4 special topics count).
    units_csc500or700_course(StudentID, UnitsCSC),
    ( UnitsCSC >= 21
    -> true
    ;  format('Fail: Need at least 21 units of CSC 500/700 electives (current = ~w).~n', [UnitsCSC]), fail ),
    % 4. Total electives: at least 30 units of graduate-level courses (500/700-level, excluding ST511 and outside 591/791, max 4 special topics).
    units_all_electives_course(StudentID, UnitsAll),
    ( UnitsAll >= 30
    -> true
    ;  format('Fail: Need at least 30 total graduate elective units (current = ~w).~n', [UnitsAll]), fail ),
    % 5. Overall GPA on all taken courses >= 3.0.
    list_taken_courses(TakenCourses, StudentID),
    ( TakenCourses \= []
    -> true
    ;  format('Fail: No completed courses found to compute GPA.~n', []), fail ),
    student_gpa(StudentID, TakenCourses, GPA),
    ( GPA >= 2.0
    -> true
    ;  format('Fail: Overall GPA must be >= 2.0 (current = ~1f).~n', [GPA]), fail ).

% MSC program graduation requirements
canGraduate_noPrint(StudentID, msc) :-
    % 1. Must pass CSC600 (Graduate Orientation).
    hasTakenCourse(StudentID, 'csc600', _Sect, _Units, G600),
    is_passing_grade(G600),
    % 2. Core courses: at least 3 core courses total, with at least 1 theory core course.
    count_theory_course(StudentID, NumTheory), 
    count_systems_course(StudentID, NumSystems),
    TotalCore is NumTheory + NumSystems,
    TotalCore >= 3, NumTheory >= 1, NumSystems >= 1,
    % 3. CSC elective courses: at least 21 units of CSC 500- or 700-level coursework (max 4 special topics count).
    units_csc500or700_course(StudentID, UnitsCSC),
    UnitsCSC >= 21,
    % 4. Total electives: at least 30 units of graduate-level courses (500/700-level, excluding ST511 and outside 591/791, max 4 special topics).
    units_all_electives_course(StudentID, UnitsAll),
    UnitsAll >= 30,
    % % 5. All CSC courses must be 500 level or above.
    % all_csc_courses_above_500(StudentID),
    % % 6. If CSC630 is taken, student must have a CSC faculty advisor (MSC program).
    % csc630_requires_csc_faculty_advisor(StudentID),
    % 7. Overall GPA on all taken courses >= 3.0.
    list_taken_courses(TakenCourses, StudentID),
    TakenCourses \= [], 
    student_gpa(StudentID, TakenCourses, GPA),
    GPA >= 2.0.
