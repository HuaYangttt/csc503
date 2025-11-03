:- consult('currentCourses.pl').
:- consult('mainUtils.pl').

% Subgoal 1: Recommend orientation (CSC600)
recommend_orientation(StudentID) :-
    ( orientation_satisfied(StudentID) ->
        true  % Orientation requirement met, nothing to recommend
    ;   % Orientation not satisfied, check status and recommend
        ( hasTakenCourse(StudentID, 'csc600', _, _, Grade) ->
            % Student took CSC600 - check if grade < 2.0 (not passing)
            ( Grade < 2.0 ->
                format('Unmet Requirement: Retake CSC600 (current grade insufficient)~n', []),
                ( currentCourse('csc600', SectionID, MinU, MaxU, _) ->
                    format('  Available course:~n', []),
                    format('  - csc600 (~w, ~w-~w units)~n', [SectionID, MinU, MaxU])
                ;   true
                )
            ;   true  % Grade >= 2.0, student passed
            )
        ;   % Student has not taken CSC600 at all
            format('Unmet Requirement: CSC600 Orientation~n', []),
            ( currentCourse('csc600', SectionID, MinU, MaxU, _) ->
                format('  Available course:~n', []),
                format('  - csc600 (~w, ~w-~w units)~n', [SectionID, MinU, MaxU])
            ;   format('  Note: CSC600 not offered in Fall 2025~n', [])
            )
        )
    ).

% Subgoal 2: Recommend core courses
recommend_core_courses(StudentID) :-
    % Count current theory and systems courses with grade >= 3.5
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
    
    TotalCore is NumTheory + NumSystems,
    
    % Calculate needs
    NeedTheory is max(0, 2 - NumTheory),
    NeedSystems is max(0, 2 - NumSystems),
    NeedTotal is max(0, 4 - TotalCore),
    
    % If needs exist, print recommendations
    ( (NeedTheory > 0 ; NeedSystems > 0) ->
        format('Unmet Requirement: Core Courses~n', []),
        ( NeedTheory > 0 ->
            format('  Need ~w more theory core course(s) with grade >=3.5~n', [NeedTheory])
        ;   true
        ),
        ( NeedSystems > 0 ->
            format('  Need ~w more systems core course(s) with grade >=3.5~n', [NeedSystems])
        ;   true
        ),
        format('  Available core courses in Fall 2025:~n', []),
        
        % Recommend theory courses if needed
        ( NeedTheory > 0 ->
            forall(
                ( currentCourse(CourseID, SectionID, MinU, MaxU, Prereq),
                  is_theory_course(CourseID),
                  meet_prerequisite(StudentID, Prereq),
                  \+ hasTakenCourse(StudentID, CourseID, _, _, _)
                ),
                format('  - ~w (~w, ~w-~w units) [Theory Core]~n', [CourseID, SectionID, MinU, MaxU])
            )
        ;   true
        ),
        
        % Recommend systems courses if needed
        ( NeedSystems > 0 ->
            forall(
                ( currentCourse(CourseID, SectionID, MinU, MaxU, Prereq),
                  is_systems_course(CourseID),
                  meet_prerequisite(StudentID, Prereq),
                  \+ hasTakenCourse(StudentID, CourseID, _, _, _)
                ),
                format('  - ~w (~w, ~w-~w units) [Systems Core]~n', [CourseID, SectionID, MinU, MaxU])
            )
        ;   true
        )
    ;   true
    ).

% Subgoal 3: Recommend 700-level courses
recommend_seven_hundred_courses(StudentID) :-
    % Count 700-level courses with grade >= 3.0
    findall(CID,
            (hasTakenCourse(StudentID, CID, _, _, Grade),
             is_700_course(CID),
             Grade >= 3.0),
            Courses700),
    sort(Courses700, UniqueCourses700),
    length(UniqueCourses700, Count700),
    
    % Calculate need
    Need700 is max(0, 2 - Count700),
    
    % If need exists, print recommendations
    ( Need700 > 0 ->
        format('Unmet Requirement: 700-level Courses~n', []),
        format('  Need ~w more 700-level course(s) with grade >=3.0~n', [Need700]),
        format('  Available 700-level courses in Fall 2025:~n', []),
        
        forall(
            ( currentCourse(CourseID, SectionID, MinU, MaxU, Prereq),
              is_700_course(CourseID),
              meet_prerequisite(StudentID, Prereq),
              \+ hasTakenCourse(StudentID, CourseID, _, _, _)
            ),
            format('  - ~w (~w, ~w-~w units)~n', [CourseID, SectionID, MinU, MaxU])
        )
    ;   true
    ).

% Subgoal 4: Recommend dissertation credits (CSC890)
recommend_dissertation_credits(StudentID) :-
    % Calculate current CSC890 units
    findall(Units,
            hasTakenCourse(StudentID, 'csc890', _, Units, _),
            UnitsList890),
    sum_list(UnitsList890, TotalUnits890),
    
    % Calculate need
    NeedUnits890 is max(0, 6 - TotalUnits890),
    
    % If need exists, print recommendations
    ( NeedUnits890 > 0 ->
        format('Unmet Requirement: Dissertation Credits (CSC890)~n', []),
        format('  Need ~w more units of CSC890~n', [NeedUnits890]),
        format('  Available option in Fall 2025:~n', []),
        
        ( currentCourse('csc890', SectionID, MinU, MaxU, Prereq) ->
            ( meet_prerequisite(StudentID, Prereq) ->
                format('  - csc890 (~w, ~w-~w units)~n', [SectionID, MinU, MaxU])
            ;   true
            )
        ;   format('  Note: CSC890 not offered in Fall 2025~n', [])
        )
    ;   true
    ).

% Subgoal 5: Recommend elective/research credits
recommend_elective_research_credits(StudentID) :-
    % Calculate current elective/research units
    units_csc_elective_research_phd(StudentID, CurrentUnits),
    
    % Calculate need
    NeedUnits is max(0, 47 - CurrentUnits),
    
    % If need exists, print recommendations
    ( NeedUnits > 0 ->
        format('Unmet Requirement: CSC Elective/Research Credits~n', []),
        format('  Need ~w more units to reach 47 total~n', [NeedUnits]),
        format('  Available courses in Fall 2025:~n', []),
        
        % Collect and categorize eligible courses
        findall(course(CourseID, SectionID, MinU, MaxU, Category),
                ( currentCourse(CourseID, SectionID, MinU, MaxU, Prereq),
                  is_cscElectivesOrResearch(CourseID),
                  meet_prerequisite(StudentID, Prereq),
                  ( \+ hasTakenCourse(StudentID, CourseID, _, _, _)
                  ; (is_800_course(CourseID))
                  ),
                  % Determine category
                  ( CourseID == 'csc890' -> Category = 'Dissertation'
                  ; member(CourseID, ['csc830', 'csc893', 'csc895', 'csc896']) -> Category = 'Research'
                  ; is_500_course(CourseID) -> Category = '500-level Elective'
                  ; is_700_course(CourseID) -> Category = '700-level Elective'
                  ; is_591or791_course(CourseID) -> Category = 'Special Topics'
                  ; Category = 'Other'
                  )
                ),
                AllCourses),
        
        % Print research courses
        format('  Research courses:~n', []),
        forall(
            member(course(CID, Sect, MinU, MaxU, Cat), AllCourses),
            ( (Cat == 'Research' ; Cat == 'Dissertation') ->
                format('    - ~w (~w, ~w-~w units)~n', [CID, Sect, MinU, MaxU])
            ;   true
            )
        ),
        
        % Print 500-level electives
        format('  500-level electives:~n', []),
        forall(
            member(course(CID, Sect, MinU, MaxU, '500-level Elective'), AllCourses),
            format('    - ~w (~w, ~w-~w units)~n', [CID, Sect, MinU, MaxU])
        ),
        
        % Print 700-level electives
        format('  700-level electives:~n', []),
        forall(
            member(course(CID, Sect, MinU, MaxU, '700-level Elective'), AllCourses),
            format('    - ~w (~w, ~w-~w units)~n', [CID, Sect, MinU, MaxU])
        ),
        
        % Print special topics
        format('  Special topics:~n', []),
        forall(
            member(course(CID, Sect, MinU, MaxU, 'Special Topics'), AllCourses),
            format('    - ~w (~w, ~w-~w units)~n', [CID, Sect, MinU, MaxU])
        ),
        
        % Print notes
        format('  Notes:~n', []),
        format('  - CSC591/791 count toward total (max 4 courses)~n', []),
        
        % Check CSC890 cap
        findall(U, hasTakenCourse(StudentID, 'csc890', _, U, _), U890List),
        sum_list(U890List, Sum890),
        ( Sum890 >= 6 ->
            format('  - CSC890 already at 6-unit cap for this calculation~n', [])
        ;   format('  - CSC890 counts toward total (max 6 units)~n', [])
        )
    ;   true
    ).

% Subgoal 6: Recommend exams
recommend_exams(StudentID) :-
    % Initialize list of exams needed
    findall(Exam,
            ( (   % Check written exam
                  ( \+ current_predicate(phdWrittenExamTaken/4)  % predicate doesn't exist
                  ; (\+ phdWrittenExamTaken(StudentID, _, _, _))  % no record for student
                  ; (phdWrittenExamTaken(StudentID, _, _, WrittenOutcome), 
                     \+ (WrittenOutcome == pass ; (number(WrittenOutcome), WrittenOutcome >= 2.0)))  % record exists but not passing
                  )
              ->  Exam = 'PhD Written Preliminary Exam'
              ;   fail
              )
            ; (   % Check oral exam
                  ( \+ current_predicate(phdOralExamTaken/4)
                  ; (\+ phdOralExamTaken(StudentID, _, _, _))
                  ; (phdOralExamTaken(StudentID, _, _, OralOutcome), 
                     \+ (OralOutcome == pass ; (number(OralOutcome), OralOutcome >= 2.0)))
                  )
              ->  Exam = 'PhD Oral Preliminary Exam'
              ;   fail
              )
            ; (   % Check defense
                  ( \+ current_predicate(phdDefenseTaken/4)
                  ; (\+ phdDefenseTaken(StudentID, _, _, _))
                  ; (phdDefenseTaken(StudentID, _, _, DefenseOutcome), 
                     \+ (DefenseOutcome == pass ; (number(DefenseOutcome), DefenseOutcome >= 2.0)))
                  )
              ->  Exam = 'PhD Dissertation Defense'
              ;   fail
              )
            ),
            ExamsNeeded),
    
    % If any exams needed, print them
    ( ExamsNeeded \= [] ->
        format('Unmet Requirement: PhD Examinations~n', []),
        format('  Exams needed:~n', []),
        forall(
            member(ExamName, ExamsNeeded),
            format('  - ~w~n', [ExamName])
        )
    ;   true
    ).

% Subgoal 7: Recommend GPA improvement
recommend_gpa_improvement(StudentID) :-
    ( overall_gpa_satisfied(StudentID) ->
        true  % GPA requirement met, nothing to recommend
    ;   % GPA not satisfied
        format('Unmet Requirement: Overall GPA~n', []),
        list_taken_courses(TakenCourses, StudentID),
        student_gpa(StudentID, TakenCourses, CurrentGPA),
        format('  Current GPA: ~2f~n', [CurrentGPA]),
        format('  Required GPA: 3.0~n', []),
        format('  Suggested actions:~n', []),
        format('  - Take additional courses to raise average~n', []),
        format('  - Focus on courses where you can achieve high grades~n', []),
        format('  - Consider strategies to improve performance in future coursework~n', [])
    ).

% Subgoal 8: Provide credit requirement notes
provide_credit_notes(StudentID) :-
    format('~n', []),
    format('=== Important Notes for Fall 2025 ===~n', []),
    format('- Total semester credits must be between 9-12 units~n', []),
    format('- Exception: Students completing dissertation defense may register for <9 units~n', []),
    format('- Prerequisites must be satisfied for all courses~n', []),
    format('- Courses cannot be retaken unless they are 800-level~n', []),
    format('- Maximum of 4 CSC591/791 courses count toward graduation~n', []),
    format('- CSC890 units are capped at 6 for elective/research credit calculation~n', []).

% main recommendation predicate for PhD students
recommendSemesterWork(StudentID, phd) :-
    % Subgoal 1: Check orientation and recommend if needed
    recommend_orientation(StudentID),
    % Subgoal 2: Check core courses and recommend if needed
    recommend_core_courses(StudentID),
    % Subgoal 3: Check 700-level courses and recommend if needed
    recommend_seven_hundred_courses(StudentID),
    % Subgoal 4: Check dissertation credits and recommend if needed
    recommend_dissertation_credits(StudentID),
    % Subgoal 5: Check elective/research credits and recommend if needed
    recommend_elective_research_credits(StudentID),
    % Subgoal 6: Check exams and recommend if needed
    recommend_exams(StudentID),
    % Subgoal 7: Check GPA and recommend if needed
    recommend_gpa_improvement(StudentID),
    % Subgoal 8: Provide final notes about credit requirements
    provide_credit_notes(StudentID).
