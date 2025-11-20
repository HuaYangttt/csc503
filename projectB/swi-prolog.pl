% input facts
% currentCourse(CourseNumber,CourseSection,MinUnits,MaxUnits,Prerequisite)/5
% hasTakenCourse(StudentID,CourseID,SectionID,Units,Grade)/5
% registrationSemester(StudentID,Program,Semester,Year,IsFirstOrNot)/5
% phdDefenseTaken(StudentID,Semester,Year,Outcome)/4
% phdOralExamTaken(StudentID,Semester,Year,Outcome)/4
% phdWrittenExamTaken(StudentID,Semester,Year,Outcome)/4
% graduateAdvisor(StudentID,AdvisorID,Program)/3
% advisoryCommitteeMember(StudentID,FacultyID)/2
% facultyAffiliation(FacultyID,HomeDepartment)/2
% planOfGraduateWorkApproved(StudentID)/1


% current course offerings (CourseNumber, Section, MinUnits, MaxUnits, Prerequisite)
currentCourse('csc885','s001',1,3,'phd').
currentCourse('csc501','s001',3,3,'csc246').
currentCourse('csc503','s001',3,3,'csc333').
currentCourse('csc505','s001',3,3,'csc316').
currentCourse('csc893','s001',1,9,'phd').
currentCourse('csc895','s001',1,9,'phd').
currentCourse('csc506','s001',3,3,'none').
currentCourse('csc510','s001',3,3,'csc316').
currentCourse('csc899','s001',1,9,'phd').
currentCourse('csc512','s001',3,3,'csc253').
currentCourse('csc517','s001',3,3,'csc326').
currentCourse('csc519','s001',3,3,'csc510').
currentCourse('csc520','s001',3,3,'csc316').
currentCourse('csc522','s001',3,3,'csc226').
currentCourse('csc533','s001',3,3,'csc316').
currentCourse('csc540','s001',3,3,'csc316').
currentCourse('csc547','s301',3,3,'csc501').
currentCourse('csc548','s001',3,3,'csc246').
currentCourse('csc554','s001',3,3,'csc316').
currentCourse('csc555','s001',3,3,'none').
currentCourse('csc561','s001',3,3,'cscmajor').
currentCourse('csc565','s001',3,3,'csc226').
currentCourse('csc574','s601',3,3,'csc316').
currentCourse('csc569','s001',3,3,'pythonlinear').
currentCourse('csc570','s001',3,3,'csc312').
currentCourse('csc572','s001',3,3,'graduate').
currentCourse('csc573','s001',3,3,'csc570').
currentCourse('csc574','s001',3,3,'csc316').
currentCourse('csc577','s001',3,3,'none').
currentCourse('csc578','s001',3,3,'none').
currentCourse('csc581','s001',3,3,'none').
currentCourse('csc591','s001',1,6,'bavg').
currentCourse('csc591','s002',1,6,'bavg').
currentCourse('csc591','s003',1,6,'bavg').
currentCourse('csc591','s004',1,6,'bavg').
currentCourse('csc591','s006',1,6,'bavg').
currentCourse('csc591','s007',1,6,'bavg').
currentCourse('csc579','s001',3,3,'csc312').
currentCourse('csc584','s001',3,3,'csc316').
currentCourse('csc595','s001',3,3,'csc574').
currentCourse('csc600','s001',1,1,'none').
currentCourse('csc714','s001',3,3,'csc451').
currentCourse('csc712','s001',3,3,'csc510').
currentCourse('csc591','s082',1,3,'bavg').
currentCourse('csc830','s001',1,3,'phd').
currentCourse('csc890','s001',1,9,'phd').
currentCourse('csc789','s001',3,3,'csc401').
currentCourse('csc791','s001',3,3,'none').
currentCourse('csc791','s002',3,3,'none').
currentCourse('csc791','s003',1,6,'none').
currentCourse('csc791','s004',1,6,'none').
currentCourse('csc791','s007',1,6,'none').
currentCourse('csc591','s008',1,6,'bavg').
currentCourse('csc591','s011',1,6,'bavg').
currentCourse('csc591','s013',1,3,'bavg').
currentCourse('csc791','s012',1,6,'none').
currentCourse('csc791','s022',1,6,'none').
currentCourse('csc791','s023',1,6,'none').
currentCourse('csc791','s024',1,6,'none').
currentCourse('csc791','s025',1,6,'none').
currentCourse('csc791','s027',1,6,'none').
currentCourse('csc591','s025',1,6,'bavg').
currentCourse('csc591','s027',1,6,'bavg').
currentCourse('csc591','s028',1,6,'bavg').



:- use_module(library(lists)).
% for list manipulation predicates (e.g., sort, member).

:- use_module(library(apply)).
% (possibly used for apply predicates, if needed)

:- use_module(library(aggregate)).
% (possibly used for aggregation predicates, if needed)


% is_passing_grade(+Grade)/1
% Checks if the numeric grade is a passing grade (3.0 or above, which corresponds roughly to a B or better).
is_passing_grade(G) :-
    (number(G), G >= 3.0).


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
    (is_400_course(CourseID); is_500_course(CourseID); is_700_course(CourseID); is_591or791_course(CourseID)),
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
    registrationSemester(StudentID, Program, _, _, _),
    Program == 'phd', !.

% Prerequisite 'bavg' requires a B average (3.0 GPA) in technical courses.
meet_prerequisite(StudentID, bavg) :-
    findall(Grade, 
            (hasTakenCourse(StudentID, Course, _Sect, _Units, Grade), is_technical_course(Course)), 
            Grades),
    Grades \= [],
    sum_list(Grades, Sum), length(Grades, Count),
    AvgGrade is Sum / Count,
    AvgGrade >= 3.0. 

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
            (hasTakenCourse(StudentID, C, _Sect, _Units, Grade), is_theory_course(C), Grade >= 3.0), 
            TheoryCourses),
    sort(TheoryCourses, UniqueTheoryCourses),
    length(UniqueTheoryCourses, NumTheoryCourses).

% count_systems_course(+StudentID, -NumSystemsCourses)
% Counts how many systems core courses the student has passed with a grade >= B.
count_systems_course(StudentID, NumSystemsCourses) :-
    findall(C, 
            (hasTakenCourse(StudentID, C, _Sect, _Units, Grade), is_systems_course(C), Grade >= 3.0), 
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

units_all_electives_course_phd(StudentID, TotalUnits) :-
    % Sum units of CSC 500/700-level courses (excluding 591/791) 
    % or specific 800-level research courses (csc830, csc893, csc895, csc896)
    findall(Units,
            (hasTakenCourse(StudentID, CID, _Sect, Units, _Grade),
             (((is_500_course(CID); is_700_course(CID)), 
               is_not_st511orother591orother791(CID));
              member(CID, ['csc830', 'csc893', 'csc895', 'csc896']))),
            UnitsListRegular),
    sum_list(UnitsListRegular, SumRegular),
    
    % Collect units from csc890, capped at 6
    findall(Units,
            hasTakenCourse(StudentID, 'csc890', _Sect, Units, _Grade),
            CSC890UnitsList),
    sum_list(CSC890UnitsList, Sum890Raw),
    (Sum890Raw > 6 -> Sum890Limited = 6 ; Sum890Limited = Sum890Raw),
    
    % Collect units of all CSC591/CSC791 courses (unlimited for PhD)
    findall(Units,
            (hasTakenCourse(StudentID, CID, _Sect, Units, _Grade),
             (CID == 'csc591'; CID == 'csc791')),
            SpecialUnitsList),
    sum_list(SpecialUnitsList, SumSpecial),
    
    TotalUnits is SumRegular + Sum890Limited + SumSpecial.

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
    registrationSemester(StudentID, _, Sem0, Y0, yes),
    current_semester(SemC, YC),
    findall(
        (Sem, Y),
        (term_seq(Sem0, Y0, SemC, YC, Sem, Y),
         \+ registrationSemester(StudentID, _, Sem, Y, _)),
        MissingTerms
    ),
    (   MissingTerms == [] 
    ->  true
    ;   fail
    ).

current_semester(fall, 2025).

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

% whyCanOrCannotTake_noPrint(+StudentID, +CourseID, +SectionID)
% True if the student can take the given course section; false otherwise.
whyCanOrCannotTake_noPrint(StudentID, CourseID, SectionID) :-

    % 2. Course must be offered
    currentCourse(CourseID, SectionID, _, MaxU, Prereq),

    % 3. Must meet prerequisite
    meet_prerequisite(StudentID, Prereq),

    % 4. Must not have taken it already (except PhD 800-level)
    ( \+ hasTakenCourse(StudentID, CourseID, SectionID, _, _)
    ; ( registrationSemester(StudentID, Program, _, _, _),
        Program == 'phd',
        is_800_course(CourseID) )
    ),

    % 5. Unit limit check
    MaxU =< 12.

% whyCanOrCannotTake(+StudentID, +CourseID, +SectionID)
% Checks whether a student can take a given course section and explains the reasons why or why not.
whyCanOrCannotTake(StudentID, CourseID, SectionID) :-
    findall(Reason, (
        (
            % 1. Not registered
            \+ registrationSemester(StudentID, _, fall, 2025, _),
            format(atom(Reason), 'Student ~w is not registered for Fall 2025', [StudentID])
        );
        (
            % 2. Course not offered
            \+ currentCourse(CourseID, SectionID, _, _, _),
            format(atom(Reason), 'Course ~w section ~w is not offered in Fall 2025', [CourseID, SectionID])
        );
        (
            % 3. Prerequisite not met
            currentCourse(CourseID, SectionID, _, _, Prereq),
            \+ meet_prerequisite(StudentID, Prereq),
            format(atom(Reason), 'Prerequisite ~w for ~w not met by student ~w', [Prereq, CourseID, StudentID])
        );
        (
            % 4. Already taken (except PhD 800)
            registrationSemester(StudentID, Program, _, _, _),
            hasTakenCourse(StudentID, CourseID, SectionID, _, _),
            \+ (Program == 'phd', is_800_course(CourseID)),
            format(atom(Reason), 'Student ~w has already taken course ~w', [StudentID, CourseID])
        );
        (
            % 5. Exceeds unit limit
            currentCourse(CourseID, SectionID, _, MaxU, _),
            MaxU > 12,
            format(atom(Reason), 'Course ~w exceeds 12-unit per semester limit (~w units)', [CourseID, MaxU])
        )
    ), Reasons),

    ( Reasons = [] ->
    format('Student ~w can enroll in ~w (section ~w).~n', [StudentID, CourseID, SectionID])
;
    format('Student ~w cannot enroll in ~w (section ~w) because:~n', [StudentID, CourseID, SectionID]),
    sort(Reasons, UniqueReasons),
    forall(member(R, UniqueReasons), format('- ~w~n', [R])),
    fail
).

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
    ( GPA >= 3.0
    -> true
    ;  format('Fail: Overall GPA must be >= 3.0 (current = ~1f).~n', [GPA]), fail ).

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
    GPA >= 3.0.


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

% recommendSemesterWork(+StudentID, +Program)
recommendSemesterWork(StudentID, msc) :-
    format('Recommendations for MSc student ~w:~n', [StudentID]),
    (   % 1) Graduation requirements met: print a message only, no course recommendations
        canGraduate_noPrint(StudentID, msc)
    ->  format('All MSc requirements are satisfied. No further courses recommended.~n', [])
    ;   % 2) Graduation requirements not met: generate a list of recommended courses (filtered by the specified rules).
        % 2.1 Count the number of Special Topics courses (CSC591/CSC791) the student has completed.
        findall(1,
                ( hasTakenCourse(StudentID, CID, _S, _U, _G),
                  (CID == 'csc591' ; CID == 'csc791')
                ),
                SpecialOnes),
        length(SpecialOnes, NumSpecialTaken),

        % 2.2 Count the number of Research courses (CSC630) the student has completed.
        findall(U,
                hasTakenCourse(StudentID, 'csc630', _S, U, _G),
                U630s),
        (   U630s == [] -> Sum630 is 0
        ;   sum_list(U630s, Sum630)
        ),

        % 2.3 Recommend courses based on the rules: select current offerings that the student has not taken and that are within the 500/700 level range.
        %     If NumSpecialTaken > 4, do not recommend CSC591/CSC791; if Sum630 > 3, do not recommend CSC630.
        %     Note: is_700_course/1 may not classify CSC791 as a 700-level course, so CSC591/CSC791 are explicitly handled here for inclusion or exclusion.
        forall(
            ( currentCourse(C, Sect, _, _, _),
              within_500_to_799(C),                  % within the 500/700 level range
              not_taken_for_recommendation(StudentID, C, Sect),  % Exclude CSC591/CSC791 by section; do not exclude CSC630; exclude all other courses by course ID.
              allow_591_791(C, NumSpecialTaken),     % Do not recommend CSC591/CSC791 if 4 or more such courses have already been taken.
              allow_630(C, Sum630),                  % Do not recommend CSC630 if 4 or more such credits have already been taken.
              whyCanOrCannotTake_noPrint(StudentID, C, Sect)    % prerequisite
            ),
            format('  ~w (~w)~n', [C, Sect])
        )
). 


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
    TotalUnits >= 72.
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
    % Subgoal 1: Orientation (CSC600)
    ( orientation_satisfied(StudentID)
    -> true
    ;  format('Fail: Student ~w did not satisfy the CSC600 orientation requirement.~n', [StudentID]),
       fail ),

    % Subgoal 2: Core courses
    ( core_courses_satisfied(StudentID)
    -> true
    ;  format('Fail: Student ~w did not satisfy the core course requirements (>=2 theory, >=2 systems, >=4 total).~n', [StudentID]),
       fail ),

    % Subgoal 3: 700-level coursework requirement
    ( seven_hundred_courses_satisfied(StudentID)
    -> true
    ;  format('Fail: Student ~w did not satisfy the requirement of taking at least two 700-level courses with passing grades.~n', [StudentID]),
       fail ),

    % Subgoal 4: Dissertation credits (CSC890 >= 6)
    ( dissertation_credits_satisfied(StudentID)
    -> true
    ;  format('Fail: Student ~w did not complete at least 6 units of CSC890 (dissertation preparation).~n', [StudentID]),
       fail ),

    % Subgoal 5: Elective + research credit requirement (>= 47)
    ( elective_research_credits_satisfied(StudentID)
    -> true
    ;  format('Fail: Student ~w does not have the required 47+ units of electives and research credits.~n', [StudentID]),
       fail ),

    % Subgoal 6: Graduate advisor in CSC
    ( advisor_satisfied(StudentID)
    -> true
    ;  format('Fail: Student ~w does not have a valid graduate advisor in CSC.~n', [StudentID]),
       fail ),

    % Subgoal 7: Advisory committee requirement
    ( advisory_committee_satisfied(StudentID)
    -> true
    ;  format('Fail: Student ~w does not meet the advisory committee requirements (4 members, >=1 outside CSC, >=2 CSC).~n', [StudentID]),
       fail ),

    % Subgoal 8: PhD Exams (all passed)
    ( exams_satisfied(StudentID)
    -> true
    ;  format('Fail: Student ~w did not pass all required PhD exams (written, oral, defense).~n', [StudentID]),
       fail ),

    % Subgoal 9: GPA >= 3.0
    ( overall_gpa_satisfied(StudentID)
    -> true
    ;  format('Fail: Student ~w does not have the minimum overall GPA of 3.0.~n', [StudentID]),
       fail ),

    % Subgoal 10: Plan of work approved
    ( plan_approved(StudentID)
    -> true
    ;  format('Fail: Student ~w does not have an approved Plan of Graduate Work.~n', [StudentID]),
       fail ).

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
                student_gpa(StudentID, TakenList, GPA), GPA < 3.0,
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
    CurrentUnits >= 72.

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

within_500_to_799(CID) :-
    atom_concat('csc', NumAtom, CID),
    atom_number(NumAtom, Num),
    Num >= 500, Num < 800.

not_taken_for_recommendation(StudentID, CID, Sect) :-
    ( CID == 'csc591' ; CID == 'csc791' ) ->
        \+ hasTakenCourse(StudentID, CID, Sect, _, _)
    ; CID == 'csc630' ->
        true
    ; \+ hasTakenCourse(StudentID, CID, _, _, _).

level_500_or_700(CID) :-
    ( is_500_course(CID) ; is_700_course(CID) ; CID == 'csc591' ; CID == 'csc791' ).

% Helper to check if CSC591/791 recommendation is allowed
allow_591_791(CID, NumSpecialTaken) :-
    ( CID == 'csc591' ; CID == 'csc791' )
    -> NumSpecialTaken < 4
    ;  true.

allow_630(CID, Sum630Units) :-
    ( CID == 'csc630' )
    -> Sum630Units < 3
    ;  true.

% Main recommendation predicate for PhD students
recommendSemesterWork(StudentID, phd) :-
    format('Recommendations for PhD student ~w:~n', [StudentID]),
    
    % Collect recommendations from each requirement sequentially
    findall(C-S, recommend_for_orientation(StudentID, C, S), L1),
    findall(C-S, recommend_for_core(StudentID, C, S), L2),
    findall(C-S, recommend_for_700level(StudentID, C, S), L3),
    findall(C-S, recommend_for_dissertation(StudentID, C, S), L4),
    findall(C-S, recommend_for_exams(StudentID, C, S), L5),
    
    % Combine recommendations so far
    append([L1, L2, L3, L4, L5], RecommendedSoFar),
    sort(RecommendedSoFar, UniqueRecommendedSoFar),
    
    % Check credit requirement LAST - if not met, add all remaining eligible courses
    (   elective_research_satisfied_check(StudentID)
    ->  FinalRecommendations = UniqueRecommendedSoFar
    ;   findall(C-S, 
                (recommend_for_credits(StudentID, C, S),
                 \+ member(C-S, UniqueRecommendedSoFar)),
                L6),
        append(UniqueRecommendedSoFar, L6, AllRecs),
        sort(AllRecs, FinalRecommendations)
    ),
    
    % Print results
    (   FinalRecommendations == []
    ->  format('All PhD requirements are satisfied. No further courses recommended.~n', [])
    ;   forall(member(C-S, FinalRecommendations),
               format('  ~w (~w)~n', [C, S]))
    ).

% 1. Orientation requirement
recommend_for_orientation(StudentID, Course, Section) :-
    \+ orientation_satisfied(StudentID),
    Course = 'csc600',
    currentCourse(Course, Section, _, _, Prereq),
    meet_prerequisite(StudentID, Prereq).

% 2. Core courses requirement
recommend_for_core(StudentID, Course, Section) :-
    \+ core_courses_satisfied(StudentID),
    currentCourse(Course, Section, _, _, Prereq),
    (is_theory_course(Course); is_systems_course(Course)),
    meet_prerequisite(StudentID, Prereq),
    \+ hasTakenCourse(StudentID, Course, _, _, _).

% 3. 700-level requirement
recommend_for_700level(StudentID, Course, Section) :-
    \+ seven_hundred_satisfied_check(StudentID),
    currentCourse(Course, Section, _, _, Prereq),
    is_700_course(Course),
    meet_prerequisite(StudentID, Prereq),
    \+ hasTakenCourse(StudentID, Course, _, _, _).

% 4. Dissertation requirement
recommend_for_dissertation(StudentID, Course, Section) :-
    \+ dissertation_satisfied_check(StudentID),
    Course = 'csc890',
    currentCourse(Course, Section, _, _, Prereq),
    meet_prerequisite(StudentID, Prereq),
    % Only if prelims not passed
    (\+ (phdWrittenExamTaken(StudentID, _, _, WR), WR == pass)
     ; \+ (phdOralExamTaken(StudentID, _, _, OR), OR == pass)).

% 5. Exams requirement
recommend_for_exams(StudentID, Course, Section) :-
    \+ exams_satisfied_check(StudentID),
    (   % Need prelims - recommend 890
        (\+ (phdWrittenExamTaken(StudentID, _, _, WR), WR == pass)
         ; \+ (phdOralExamTaken(StudentID, _, _, OR), OR == pass)),
        Course = 'csc890',
        currentCourse(Course, Section, _, _, Prereq),
        meet_prerequisite(StudentID, Prereq)
    ;   % Prelims passed, need defense - recommend 899
        (phdWrittenExamTaken(StudentID, _, _, WR), WR == pass),
        (phdOralExamTaken(StudentID, _, _, OR), OR == pass),
        \+ (phdDefenseTaken(StudentID, _, _, DR), DR == pass),
        Course = 'csc899',
        currentCourse(Course, Section, _, _, Prereq),
        meet_prerequisite(StudentID, Prereq)
    ).

% 6. Credit requirement - all eligible courses
recommend_for_credits(StudentID, Course, Section) :-
    currentCourse(Course, Section, _, _, Prereq),
    (is_cscElectivesOrResearch(Course); Course == 'csc591'; Course == 'csc791'),
    meet_prerequisite(StudentID, Prereq),
    % Exam-based filtering
    (   Course == 'csc890' 
    ->  (\+ (phdWrittenExamTaken(StudentID, _, _, WR), WR == pass)
         ; \+ (phdOralExamTaken(StudentID, _, _, OR), OR == pass))
    ;   Course == 'csc899'
    ->  (phdWrittenExamTaken(StudentID, _, _, WR), WR == pass),
        (phdOralExamTaken(StudentID, _, _, OR), OR == pass)
    ;   true
    ),
    % Already taken check
    (   is_800_course(Course) -> true
    ;   (Course == 'csc591'; Course == 'csc791') 
    ->  \+ hasTakenCourse(StudentID, Course, Section, _, _)
    ;   \+ hasTakenCourse(StudentID, Course, _, _, _)
    ).

% Default fact predicates - fail if no facts exist in knowledge base
phdWrittenExamTaken(_, _, _, _) :- fail.
phdOralExamTaken(_, _, _, _) :- fail.
phdDefenseTaken(_, _, _, _) :- fail.
hasTakenCourse(_, _, _, _, _) :- fail.
registrationSemester(_, _, _, _, _) :- fail.
currentCourse(_, _, _, _, _) :- fail.
graduateAdvisor(_, _, _) :- fail.
advisoryCommitteeMember(_, _) :- fail.
facultyAffiliation(_, _) :- fail.
planOfGraduateWorkApproved(_) :- fail.
