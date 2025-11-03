
% input facts
currentCourse(CourseNumber,CourseSection,MinUnits,MaxUnits,Prerequisite)/5
hasTakenCourse(StudentID,CourseID,SectionID,Units,Grade)/5
registrationSemester(StudentID,Program,Semester,Year,IsFirstOrNot)/5
phdDefenseTaken(StudentID,Semester,Year,Outcome)/4
phdOralExamTaken(StudentID,Semester,Year,Outcome)/4
phdWrittenExamTaken(StudentID,Semester,Year,Outcome)/4
graduateAdvisor(StudentID,AdvisorID,Program)/3
advisoryCommitteeMember(StudentID,FacultyID)/2
facultyAffiliation(FacultyID,HomeDepartment)/2
planOfGraduateWorkApproved(StudentID)/1

:- use_module(library(lists)).
% for list manipulation predicates (e.g., sort, member).

:- use_module(library(apply)).
% (possibly used for apply predicates, if needed)

:- use_module(library(aggregate)).
% (possibly used for aggregation predicates, if needed)


% is_passing_grade(+Grade)/1
% Checks if the numeric grade is a passing grade (2.0 or above, which corresponds roughly to a B or better).
is_passing_grade(G) :-
    (number(G), G >= 2.0).


% is_400_course(+CourseID)/1
% True if CourseID is a 400-level course (course number between 400 and 499).
is_400_course(CourseID) :-
    atom_concat('csc', NumberAtom, CourseID),
    atom_number(NumberAtom, Number),
    Number >= 400, Number < 500.

% is_500_course(+CourseID)/1
% True if CourseID is a 500-level course (course number between 500 and 599, excluding 591). csc is used bc there is only csc course
is_500_course(CourseID) :-
    atom_concat('csc', NumberAtom, CourseID),
    atom_number(NumberAtom, Number),
    Number >= 500, Number < 600,
    Number \= 591.

% is_csc_course(+CourseID)/1
% True if CourseID is a Computer Science (CSC) course. 
is_csc_course(CourseID) :-
    sub_atom(CourseID, 0, 3, _, 'csc'). 

% is_theory_course(+CourseID)/1
% True if CourseID is one of the designated theory core courses.
% Theory core courses include: csc503, csc505, csc512, csc514, csc565, csc579, csc580, csc707.
is_theory_course(CourseID) :-
    member(CourseID, ['csc503', 'csc505', 'csc512', 'csc514', 'csc565', 'csc579', 'csc580', 'csc707']).

% is_systems_course(+CourseID)/1
% True if CourseID is one of the designated systems core courses.
% Systems core courses include: csc501, csc506, csc510, csc520, csc540, csc561, csc570, csc574.
% Note: csc720 may substitute for csc520; csc573 may substitute for csc570 (included here).
is_systems_course(CourseID) :-
    member(CourseID, ['csc501', 'csc506', 'csc510', 'csc520', 'csc540', 'csc561', 'csc570', 'csc574', 'csc573', 'csc720']).

% is_700_course(+CourseID)/1
% True if CourseID is a 700-level course (course number between 700 and 799, excluding 791).
is_700_course(CourseID) :-
    atom_concat('csc', NumberAtom, CourseID),
    atom_number(NumberAtom, Number),
    Number >= 700, Number < 800,
    Number \= 791.

% is_800_course(+CourseID)/1
% True if CourseID is an 800-level course (course number between 800 and 899).
is_800_course(CourseID) :-
    atom_concat('csc', NumberAtom, CourseID),
    atom_number(NumberAtom, Number),
    Number >= 800, Number < 900.

% is_591or791_course(+CourseID)/1
% True if CourseID is CSC591 or CSC791 (special topics/independent study courses).
is_591or791_course(CourseID) :-
    CourseID == 'csc591'; CourseID == 'csc791'.

% is_cscElectivesOrResearch(+CourseID)/1
% True if CourseID counts as a CSC elective or research course for PhD requirements.
% This includes CSC courses at the 500 or 700 level, and specific research courses (csc830, csc890, csc893, csc895, csc896).
is_cscElectivesOrResearch(CourseID) :-
    (is_csc_course(CourseID), (is_500_course(CourseID); is_700_course(CourseID)));
    member(CourseID, ['csc830', 'csc890', 'csc893', 'csc895', 'csc896']).

% is_technical_course(+CourseID)/1
% True if CourseID is considered a "technical course".
is_technical_course(CourseID) :-
    % A technical course is any course at the graduate level (400 or above) 
    % that is not ST511 and not a 591/791 from another department.
    (is_400_course(CourseID); is_500_course(CourseID); is_700_course(CourseID); is_800_course(CourseID); is_591or791_course(CourseID)),
    is_not_st511orother591orother791(CourseID).

% is_not_st511orother591orother791(+CourseID)
% True if CourseID is not ST511 and not a 591/791 course from outside the CSC department.
is_not_st511orother591orother791(CourseID) :-
    CourseID \= 'st511',
    \+ (sub_atom(CourseID, _, 3, 0, '591'), \+ sub_atom(CourseID, 0, 3, _, 'csc')),
    \+ (sub_atom(CourseID, _, 3, 0, '791'), \+ sub_atom(CourseID, 0, 3, _, 'csc')).

% list_current_courses(-ListCurrentCourseIDs)
% Retrieves a sorted list of all CourseIDs offered in the current semester (from currentCourse facts).
list_current_courses(ListCurrentCourseIDs) :-
    findall(CourseID, currentCourse(CourseID, _, _, _, _), CourseList),
    msort(CourseList, ListCurrentCourseIDs).

% list_taken_courses(-ListTakenCourseIDs, +StudentID)
% Retrieves a sorted list of all CourseIDs that the student has taken.
list_taken_courses(ListTakenCourseIDs, StudentID) :-
    findall(CID, hasTakenCourse(StudentID, CID, _Section, _Units, _Grade), Courses),
    sort(Courses, ListTakenCourseIDs).

% student_gpa(+StudentID, +CourseIDs, -GPA)
% Computes the average numeric grade of the StudentID over the given list of CourseIDs.
% The GPA returned is the arithmetic mean of the numeric grades for those courses.
student_gpa(StudentID, CourseIDs, GPA) :-
    findall(Numeric,
        (member(C, CourseIDs),
         hasTakenCourse(StudentID, C, _Section, _Units, Grade),
         number(Grade),
         Numeric = Grade),
        NumericGrades),
    NumericGrades \= [],
    sum_list(NumericGrades, Sum),
    length(NumericGrades, Count),
    GPA is Sum / Count.

% meet_prerequisite(+StudentID, +CourseID, +MinGrade)
% Checks if StudentID has taken CourseID with a grade >= MinGrade.
meet_prerequisite(StudentID, CourseID, MinGrade) :-
    hasTakenCourse(StudentID, CourseID, _Section, _Units, Grade),
    number(MinGrade),
    Grade >= MinGrade.

% meet_prerequisite(+StudentID, +Prerequisite)
% Checks if StudentID meets the given prerequisite (which could be a special token or course ID).
% Special prerequisites: none, phd, bavg, cscmajor, graduate.
% Course prerequisites: require that the course was taken with a passing grade.
 
% Prerequisite 'none' is always satisfied.
meet_prerequisite(_StudentID, none) :- !.

% Prerequisite 'phd' requires the student to be in the PhD program in Fall 2025.
meet_prerequisite(StudentID, phd) :-
    registrationSemester(StudentID, Program, Semester, Year, _),
    Semester == 'fall', Year == 2025,
    Program == 'phd'.

% Prerequisite 'bavg' requires a B average (3.0 GPA) in technical courses.
meet_prerequisite(StudentID, bavg) :-
    findall(Grade, 
            (hasTakenCourse(StudentID, Course, _Sect, _Units, Grade), is_technical_course(Course)), 
            Grades),
    Grades \= [],
    sum_list(Grades, Sum), length(Grades, Count),
    AvgGrade is Sum / Count,
    AvgGrade >= 2.0. 

% Prerequisite 'cscmajor' requires the student to be a CSC graduate student (MSC or PhD).
meet_prerequisite(StudentID, cscmajor) :-
    registrationSemester(StudentID, Program, Semester, Year, _),
    Semester == 'fall', Year == 2025,
    (Program == 'msc'; Program == 'phd').

% Prerequisite 'graduate' requires the student to be a graduate student (MSC or PhD).
meet_prerequisite(StudentID, graduate) :-
    registrationSemester(StudentID, Program, Semester, Year, _),
    Semester == 'fall', Year == 2025,
    (Program == 'msc'; Program == 'phd').

% If the prerequisite is a CourseID (normal course), check that the student has taken it with a passing grade.
meet_prerequisite(StudentID, PrereqCourse) :-
    \+ member(PrereqCourse, [none, phd, bavg, cscmajor, graduate]),
    hasTakenCourse(StudentID, PrereqCourse, _Section, _Units, Grade),
    is_passing_grade(Grade).

% count_theory_course(+StudentID, -NumTheoryCourses)
% Counts how many theory core courses the student has passed with a grade >= B.
count_theory_course(StudentID, NumTheoryCourses) :-
    findall(C, 
            (hasTakenCourse(StudentID, C, _Sect, _Units, Grade), is_theory_course(C), Grade >= 2.0), 
            TheoryCourses),
    sort(TheoryCourses, UniqueTheoryCourses),
    length(UniqueTheoryCourses, NumTheoryCourses).

% count_systems_course(+StudentID, -NumSystemsCourses)
% Counts how many systems core courses the student has passed with a grade >= B.
count_systems_course(StudentID, NumSystemsCourses) :-
    findall(C, 
            (hasTakenCourse(StudentID, C, _Sect, _Units, Grade), is_systems_course(C), Grade >= 2.0), 
            SystemsCourses),
    sort(SystemsCourses, S0),
    normalize_subs(S0, UniqueSystemsCourses),
    length(UniqueSystemsCourses, NumSystemsCourses).

normalize_subs(S0, S) :-
    ( ord_memberchk(csc720, S0), ord_memberchk(csc520, S0)
      -> ord_del_element(S0, csc720, S1)
      ;  S1 = S0 ),
    ( ord_memberchk(csc573, S1), ord_memberchk(csc570, S1)
      -> ord_del_element(S1, csc573, S)
      ;  S = S1 ).

% units_csc500or700_course(+StudentID, -TotalUnits)
% Sums the total units of CSC 500- or 700-level courses the student has taken (with at most 4 courses of CSC591/CSC791 counted).
units_csc500or700_course(StudentID, TotalUnits) :-
    % Sum units of all CSC 500/700-level courses (excluding 591/791).
    findall(Units, 
            (hasTakenCourse(StudentID, CID, _Sect, Units, _Grade), 
             is_csc_course(CID), (is_500_course(CID); is_700_course(CID))), 
            UnitsListRegular),
    sum_list(UnitsListRegular, SumRegular),
    % Collect units of all CSC591/CSC791 courses taken.
    findall(Units, 
            (hasTakenCourse(StudentID, CID, _Sect, Units, _Grade), 
             (CID == 'csc591'; CID == 'csc791')), 
            SpecialUnitsList),
    msort(SpecialUnitsList, SortedSpecial),   % sort units in ascending order
    length(SortedSpecial, CountSpecial),
    ( CountSpecial > 4 ->
        % If more than 4 special topic courses, only count the 4 with highest units.
        DropCount is CountSpecial - 4,
        length(DropList, DropCount),
        append(DropList, TopFour, SortedSpecial),
        sum_list(TopFour, SumSpecialLimited)
    ; 
        sum_list(SortedSpecial, SumSpecialLimited)
    ),
    TotalUnits is SumRegular + SumSpecialLimited.

% units_all_electives_course(+StudentID, -TotalUnits)
% Sums the total units of all elective courses the student has taken for MS (500/700-level courses, excluding ST511 and non-CSC 591/791, with at most 4 special topics counted).
units_all_electives_course(StudentID, TotalUnits) :-
    % Sum units of all 500/700-level courses (excluding any 591/791 by number) that are not ST511 or outside 591/791.
    findall(Units,
            (hasTakenCourse(StudentID, CID, _Sect, Units, _Grade),
             (is_500_course(CID); is_700_course(CID)),
             is_not_st511orother591orother791(CID)),
            UnitsListRegular),
    sum_list(UnitsListRegular, SumRegular),
    % Collect units of all CSC591/CSC791 courses taken (special topics in CSC) (maximum 4).
    findall(Units,
            (hasTakenCourse(StudentID, CID, _Sect, Units, _Grade),
             (CID == 'csc591'; CID == 'csc791')),
            SpecialUnitsList),
    msort(SpecialUnitsList, SortedSpecial),
    length(SortedSpecial, CountSpecial),
    ( CountSpecial > 4 ->
        DropCount is CountSpecial - 4,
        length(DropList, DropCount),
        append(DropList, TopFour, SortedSpecial),
        sum_list(TopFour, SumSpecialLimited)
    ;
        sum_list(SortedSpecial, SumSpecialLimited)
    ),
    % Collect units of all CSC630 courses taken (maximum 3)
    findall(Units,
            hasTakenCourse(StudentID, 'csc630', _Sect, Units, _Grade),
            CSC630UnitsList),
    sum_list(CSC630UnitsList, SumCSC630Raw),
    ( SumCSC630Raw > 3 -> SumCSC630 = 3 ; SumCSC630 = SumCSC630Raw ),
    TotalUnits is SumRegular + SumSpecialLimited + SumCSC630.

% all_csc_courses_above_500(+StudentID)
all_csc_courses_above_500(StudentID) :-
    % Collect all CSC courses the student has taken.
    findall(CID,
            ( hasTakenCourse(StudentID, CID, _Sect, _Units, _Grade),
              is_csc_course(CID)
            ),
            CSCCourses),
    % Ensure every CSC course number is >= 500.
    forall(
        member(CID, CSCCourses),
        (
            % Extract the numeric part after "csc"
            atom_concat('csc', NumAtom, CID),
            atom_number(NumAtom, Num),
            % Verify course number is 500 or higher
            Num >= 500
        )
    ).

% csc630_requires_csc_faculty_advisor(+StudentID)
csc630_requires_csc_faculty_advisor(StudentID) :-
    (   % If CSC630 is taken, enforce advisor constraint
        hasTakenCourse(StudentID, 'csc630', _Sect, _Units, _Grade)
    ->  % First, check that required predicates exist
        predicate_property(graduateAdvisor(_,_,_), defined),
        predicate_property(facultyAffiliation(_,_), defined),
        % Now safely check the conditions
        graduateAdvisor(StudentID, Advisor, Program),
        Program == 'msc',
        facultyAffiliation(Advisor, Dept),
        Dept == 'csc'
    ;   % If CSC630 is not taken, succeed automatically
        true
    ).

% units_csc_elective_research_phd(+StudentID, -TotalUnits)
% Sums the total units of CSC elective and research courses the PhD student has taken (at least 47 required, with special topic and csc890 limits).
units_csc_elective_research_phd(StudentID, TotalUnits) :-
    % Sum units of all CSC 500/700-level courses (excluding 591/791) and specific research courses (except csc890).
    findall(Units,
            (hasTakenCourse(StudentID, CID, _Sect, Units, _Grade),
             ((is_csc_course(CID), (is_500_course(CID); is_700_course(CID)))
               ; member(CID, ['csc830', 'csc893', 'csc895', 'csc896'])
             )
            ),
            UnitsListRegular),
    sum_list(UnitsListRegular, SumRegular),
    % Handle CSC890 units separately (count maximum 6 units of csc890).
    findall(U, hasTakenCourse(StudentID, 'csc890', _Sect, U, _Grade), List890),
    sum_list(List890, Sum890), (Sum890 > 6 -> Count890 = 6; Count890 = Sum890),
    % Handle CSC591/CSC791 units (limit 4 courses by units).
    findall(U,
            (hasTakenCourse(StudentID, CID, _Sect, U, _Grade),
             (CID == 'csc591'; CID == 'csc791')),
            SpecialUnits),
    sort(SpecialUnits, SortedSpecial),
    length(SortedSpecial, CountSpecial),
    ( CountSpecial > 4 ->
        DropCount is CountSpecial - 4,
        length(DropList, DropCount),
        append(DropList, TopFour, SortedSpecial),
        sum_list(TopFour, SumSpecialLimited)
    ;
        sum_list(SortedSpecial, SumSpecialLimited)
    ),
    TotalUnits is SumRegular + Count890 + SumSpecialLimited.

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
