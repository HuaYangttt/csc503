:- consult('currentCourses.pl').
:- consult('mainUtils.pl').

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