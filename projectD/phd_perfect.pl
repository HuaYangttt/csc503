:- consult('swi-prolog.pl').
:- consult('phdCanGraduate.pl').
:- consult('phdTerminated.pl').
:- consult('whyCanOrCannotTake.pl').
:- consult('phdRecommend.pl').

% registration history
registrationSemester(phd003, phd, fall, 2020, yes).
registrationSemester(phd003, phd, spring, 2021, no).
registrationSemester(phd003, phd, fall, 2021, no).
registrationSemester(phd003, phd, spring, 2022, no).
registrationSemester(phd003, phd, fall, 2022, no).
registrationSemester(phd003, phd, spring, 2023, no).
registrationSemester(phd003, phd, fall, 2023, no).
registrationSemester(phd003, phd, spring, 2024, no).

% Fall 2020 (10 credits)
% Orientation course
hasTakenCourse(phd003, csc600, s001, 1, 3.0).
% Theory core
hasTakenCourse(phd003, csc505, s001, 3, 4.0).  
% Systems core
hasTakenCourse(phd003, csc501, s001, 3, 4.0).  
hasTakenCourse(phd003, csc540, s001, 3, 4.0).  

% Spring 2021 (12 credits)
% Theory core
hasTakenCourse(phd003, csc503, s001, 3, 4.0).  
% 700~ level
hasTakenCourse(phd003, csc714, s001, 3, 4.0).  
hasTakenCourse(phd003, csc712, s001, 3, 4.0). 
% research credits
hasTakenCourse(phd003, csc830, s001, 3, 4.0).  

% Fall 2021 (9 credits)
% research credits
hasTakenCourse(phd003, csc830, s001, 3, 4.0).  
% written prelim
hasTakenCourse(phd003, csc890, s001, 6, 4.0).  

% Spring 2022 (12 credits)
% research credits
hasTakenCourse(phd003, csc830, s001, 3, 4.0).  
hasTakenCourse(phd003, csc893, s001, 9, 4.0).

% fall 2022 (9 credits, but only 3 credits toward graduaction)
% oral prelim
hasTakenCourse(phd003, csc890, s001, 6, 4.0).
% research credits
hasTakenCourse(phd003, csc830, s001, 3, 4.0).

% Spring 2023 (9 credits)
% research credits
hasTakenCourse(phd003, csc893, s001, 9, 4.0).

% fall 2023 (9 credits)
% research credits
hasTakenCourse(phd003, csc893, s001, 9, 4.0).

% Spring 2024 (9 credits)
% dissertation
hasTakenCourse(phd003, csc895, s001, 9, 4.0).

% Exam outcomes
phdWrittenExamTaken(phd003, fall, 2021, 4.0).
phdOralExamTaken(phd003, fall, 2022, 4.0).
phdDefenseTaken(phd003, spring, 2024, 4.0).

% Graduate advisor
graduateAdvisor(phd003, advisor1, phd).

% Advisory committee (4 members)
advisoryCommitteeMember(phd003, advisor1).
advisoryCommitteeMember(phd003, member2).
advisoryCommitteeMember(phd003, member3).
advisoryCommitteeMember(phd003, member4).

% Faculty affiliations
facultyAffiliation(advisor1, csc).
facultyAffiliation(member2, csc).
facultyAffiliation(member3, ece).  % Outside CS
facultyAffiliation(member4, csc).

% Plan approved
planOfGraduateWorkApproved(phd003).