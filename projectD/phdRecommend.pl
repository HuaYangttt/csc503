:- consult('currentCourses.pl').
:- consult('mainUtils.pl').

% Helper predicates to check if each requirement is satisfied
orientation_satisfied_check(StudentID) :-
    orientation_satisfied(StudentID).

core_courses_satisfied_check(StudentID) :-
    findall(CID,
            (hasTakenCourse(StudentID, CID, _, _, Grade),
             is_theory_course(CID),
             Grade >= 3.5),
            TheoryCourses),
    sort(TheoryCourses, UniqueTheory),
    length(UniqueTheory, NumTheory),
    
    findall(CID,
            (hasTakenCourse(StudentID, CID, _, _, Grade),
             is_systems_course(CID),
             Grade >= 3.5),
            SystemsCourses),
    sort(SystemsCourses, UniqueSystems),
    length(UniqueSystems, NumSystems),
    
    NumTheory >= 2,
    NumSystems >= 2.

seven_hundred_satisfied_check(StudentID) :-
    findall(CID,
            (hasTakenCourse(StudentID, CID, _, _, Grade),
             is_700_course(CID),
             Grade >= 3.0),
            Courses700),
    sort(Courses700, UniqueCourses700),
    length(UniqueCourses700, Count700),
    Count700 >= 2.

dissertation_satisfied_check(StudentID) :-
    findall(Units,
            hasTakenCourse(StudentID, 'csc890', _, Units, _),
            UnitsList890),
    sum_list(UnitsList890, TotalUnits890),
    TotalUnits890 >= 6.

elective_research_satisfied_check(StudentID) :-
    units_csc_elective_research_phd(StudentID, CurrentUnits),
    CurrentUnits >= 47.

exams_satisfied_check(StudentID) :-
    % Check written exam
    ( current_predicate(phdWrittenExamTaken/4) ->
        ( phdWrittenExamTaken(StudentID, _, _, WrittenOutcome) ->
            (WrittenOutcome == pass ; (number(WrittenOutcome), WrittenOutcome >= 2.0))
        ;   fail
        )
    ;   fail
    ),
    % Check oral exam
    ( current_predicate(phdOralExamTaken/4) ->
        ( phdOralExamTaken(StudentID, _, _, OralOutcome) ->
            (OralOutcome == pass ; (number(OralOutcome), OralOutcome >= 2.0))
        ;   fail
        )
    ;   fail
    ),
    % Check defense
    ( current_predicate(phdDefenseTaken/4) ->
        ( phdDefenseTaken(StudentID, _, _, DefenseOutcome) ->
            (DefenseOutcome == pass ; (number(DefenseOutcome), DefenseOutcome >= 2.0))
        ;   fail
        )
    ;   fail
    ).

gpa_satisfied_check(StudentID) :-
    overall_gpa_satisfied(StudentID).

% Check if all PhD requirements are satisfied
all_phd_requirements_satisfied(StudentID) :-
    orientation_satisfied_check(StudentID),
    core_courses_satisfied_check(StudentID),
    seven_hundred_satisfied_check(StudentID),
    dissertation_satisfied_check(StudentID),
    elective_research_satisfied_check(StudentID),
    exams_satisfied_check(StudentID),
    gpa_satisfied_check(StudentID).

% Helper to check if CSC591/791 recommendation is allowed
allow_591_791(CID, NumSpecialTaken) :-
    ( CID == 'csc591' ; CID == 'csc791' )
    -> NumSpecialTaken < 4
    ;  true.

% Main recommendation predicate for PhD students
recommendSemesterWork(StudentID, phd) :-
    format('Recommendations for PhD student ~w:~n', [StudentID]),
    (   % All requirements met: print message only, no course recommendations
        all_phd_requirements_satisfied(StudentID)
    ->  format('All PhD requirements are satisfied. No further courses recommended.~n', [])
    ;   % Requirements not met: generate list of recommended courses
        
        % Count the number of Special Topics courses (CSC591/CSC791) the student has completed
        findall(1,
                ( hasTakenCourse(StudentID, CID, _S, _U, _G),
                  (CID == 'csc591' ; CID == 'csc791')
                ),
                SpecialOnes),
        length(SpecialOnes, NumSpecialTaken),
        
        % List eligible courses
        forall(
            ( currentCourse(CourseID, SectionID, _, _, Prereq),
              is_cscElectivesOrResearch(CourseID),
              meet_prerequisite(StudentID, Prereq),
              ( \+ hasTakenCourse(StudentID, CourseID, _, _, _)
              ; is_800_course(CourseID)
              ),
              allow_591_791(CourseID, NumSpecialTaken)  % Don't recommend CSC591/791 if already taken 4
            ),
            format('  ~w (~w)~n', [CourseID, SectionID])
        )
    ).