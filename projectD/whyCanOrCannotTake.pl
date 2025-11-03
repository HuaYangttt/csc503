:- consult('currentCourses.pl').
:- consult('mainUtils.pl').

% whyCanOrCannotTake_noPrint(+StudentID, +CourseID, +SectionID)
% True if the student can take the given course section; false otherwise.
whyCanOrCannotTake_noPrint(StudentID, CourseID, SectionID) :-
    % 1. Must be registered for Fall 2025
    registrationSemester(StudentID, _, fall, 2025, _),

    % 2. Course must be offered
    currentCourse(CourseID, SectionID, _, MaxU, Prereq),

    % 3. Must meet prerequisite
    meet_prerequisite(StudentID, Prereq),

    % 4. Must not have taken it already (except PhD 800-level)
    ( \+ hasTakenCourse(StudentID, CourseID, _, _, _)
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
            hasTakenCourse(StudentID, CourseID, _, _, _),
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