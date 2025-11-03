:- consult('currentCourses.pl').
:- consult('mainUtils.pl').

% Subgoal 1: Orientation satisfied (CSC600 with passing grade)
orientation_satisfied(StudentID) :-
    hasTakenCourse(StudentID, 'csc600', _, _, Grade),
    is_passing_grade(Grade).
% Subgoal 2: Core courses satisfied (>=2 theory, >=2 systems, total >=4, with grade >= 3.5)
core_courses_satisfied(StudentID) :-
    findall(CID,
            (hasTakenCourse(StudentID, CID, _, _, Grade),
             is_theory_course(CID),
             Grade >= 3.5),
            TheoryCourses),
    sort(TheoryCourses, UniqueTheory),
    length(UniqueTheory, NumTheory),
    NumTheory >= 2,
    findall(CID,
            (hasTakenCourse(StudentID, CID, _, _, Grade),
             is_systems_course(CID),
             Grade >= 3.5),
            SystemsCourses),
    sort(SystemsCourses, UniqueSystems),
    length(UniqueSystems, NumSystems),
    NumSystems >= 2,
    TotalCore is NumTheory + NumSystems,
    TotalCore >= 4.
% Subgoal 3: 700-level courses satisfied (>=2 passed with grade >= 3.0)
seven_hundred_courses_satisfied(StudentID) :-
    findall(CID,
            (hasTakenCourse(StudentID, CID, _, _, Grade),
             is_700_course(CID),
             Grade >= 3.0),
            Courses700),
    sort(Courses700, UniqueCourses700),
    length(UniqueCourses700, Count700),
    Count700 >= 2.
% Subgoal 4: Dissertation credits satisfied (>=6 units of CSC890)
dissertation_credits_satisfied(StudentID) :-
    findall(Units,
            hasTakenCourse(StudentID, 'csc890', _, Units, _),
            UnitsList890),
    sum_list(UnitsList890, TotalUnits890),
    TotalUnits890 >= 6.
% Subgoal 5: Elective/research credits satisfied (>=47 units)
elective_research_credits_satisfied(StudentID) :-
    units_csc_elective_research_phd(StudentID, TotalUnits),
    TotalUnits >= 47.
% Subgoal 6: Advisor satisfied (graduate advisor must be CSC faculty)
advisor_satisfied(StudentID) :-
    graduateAdvisor(StudentID, Advisor, phd),
    facultyAffiliation(Advisor, 'csc').
% Subgoal 7: Advisory committee satisfied (>=4 members, >=2 CSC, >=1 outside CSC)
advisory_committee_satisfied(StudentID) :-
    findall(Faculty,
            advisoryCommitteeMember(StudentID, Faculty),
            CommitteeMembers),
    length(CommitteeMembers, TotalMembers),
    TotalMembers >= 4,
    findall(1,
            (member(Faculty, CommitteeMembers),
             facultyAffiliation(Faculty, 'csc')),
            CSCMembers),
    length(CSCMembers, CSCCount),
    CSCCount >= 2,
    findall(1,
            (member(Faculty, CommitteeMembers),
             facultyAffiliation(Faculty, Dept),
             Dept \= 'csc'),
            OutsideCSCMembers),
    length(OutsideCSCMembers, OutsideCSCCount),
    OutsideCSCCount >= 1.
% Subgoal 8: Exams satisfied (written, oral, and defense all passed)
exams_satisfied(StudentID) :-
    (current_predicate(phdWrittenExamTaken/4) -> 
        (phdWrittenExamTaken(StudentID, _, _, WrittenOutcome),
         (WrittenOutcome == pass ; (number(WrittenOutcome), WrittenOutcome >= 2.0)))
    ;   fail
    ),
    (current_predicate(phdOralExamTaken/4) -> 
        (phdOralExamTaken(StudentID, _, _, OralOutcome),
         (OralOutcome == pass ; (number(OralOutcome), OralOutcome >= 2.0)))
    ;   fail
    ),
    (current_predicate(phdDefenseTaken/4) -> 
        (phdDefenseTaken(StudentID, _, _, DefenseOutcome),
         (DefenseOutcome == pass ; (number(DefenseOutcome), DefenseOutcome >= 2.0)))
    ;   fail
    ).
% Subgoal 9: Overall GPA satisfied (>=83, which is B average or 3.0)
overall_gpa_satisfied(StudentID) :-
    list_taken_courses(TakenCoursesList, StudentID),
    student_gpa(StudentID, TakenCoursesList, GPA),
    GPA >= 3.0.
% Subgoal 10: Plan approved (plan of graduate work must be approved)
plan_approved(StudentID) :-
    planOfGraduateWorkApproved(StudentID).

% PhD program graduation requirements
canGraduate(StudentID, phd) :-
    % Subgoal 1: Check CSC600 orientation requirement
    orientation_satisfied(StudentID),
    % Subgoal 2: Check core course requirements (>=2 theory, >=2 systems, total >=4)
    core_courses_satisfied(StudentID),
    % Subgoal 3: Check 700-level course requirement (>=2 passed)
    seven_hundred_courses_satisfied(StudentID),
    % Subgoal 4: Check dissertation credits (>=6 units of CSC890)
    dissertation_credits_satisfied(StudentID),
    % Subgoal 5: Check CSC elective/research credits (>=47 units)
    elective_research_credits_satisfied(StudentID),
    % Subgoal 6: Check graduate advisor requirement (CSC faculty)
    advisor_satisfied(StudentID),
    % Subgoal 7: Check advisory committee (>=4 members, >=1 outside CSC, >=2 CSC)
    advisory_committee_satisfied(StudentID),
    % Subgoal 8: Check PhD exams (written, oral, defense all passed)
    exams_satisfied(StudentID),
    % Subgoal 9: Check overall GPA (>=3.0)
    overall_gpa_satisfied(StudentID),
    % Subgoal 10: Check plan of graduate work approved
    plan_approved(StudentID).
