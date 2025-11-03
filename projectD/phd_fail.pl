: - consult('swi-prolog.pl').
:- consult('phdCanGraduate.pl').
:- consult('phdTerminated.pl').
:- consult('whyCanOrCannotTake.pl').
:- consult('phdRecommend.pl').

% registration history
registrationSemester(phd003, phd, fall, 2022, yes).
registrationSemester(phd003, phd, spring, 2023, no).
registrationSemester(phd003, phd, fall, 2023, no).
registrationSemester(phd003, phd, spring, 2024, no).
registrationSemester(phd003, phd, fall, 2024, no).
registrationSemester(phd003, phd, spring, 2025, no).
registrationSemester(phd003, phd, fall, 2025, no).

% Fall 2022 (10 credits) - First semester
% Orientation course (S/U)
hasTakenCourse(phd003, csc600, s001, 1, 3.0).
% Core - Theory
hasTakenCourse(phd003, csc505, s001, 3, 4.0).  
hasTakenCourse(phd003, csc503, s001, 3, 4.0).
% Core - Systems
hasTakenCourse(phd003, csc501, s001, 3, 4.0).  

% Spring 2023 (9 credits)
% Core - Systems
hasTakenCourse(phd003, csc540, s001, 3, 4.0).
% 700-level courses
hasTakenCourse(phd003, csc714, s001, 3, 4.0).  
hasTakenCourse(phd003, csc712, s001, 3, 4.0). 

% Fall 2023 (12 credits)
% Written Prelim (S/U)
hasTakenCourse(phd003, csc890, s001, 3, 3.0).
% Electives
hasTakenCourse(phd003, csc520, s001, 3, 4.0).  
hasTakenCourse(phd003, csc570, s001, 3, 4.0).

% Spring 2024 (9 credits)
% Electives
hasTakenCourse(phd003, csc574, s001, 3, 4.0).   
hasTakenCourse(phd003, csc565, s001, 3, 4.0).
% Oral Prelim (S/U)
hasTakenCourse(phd003, csc890, s001, 3, 3.0).

% Fall 2024 (9 credits)
% Research credits (S/U)
hasTakenCourse(phd003, csc893, s001, 6, 3.0).
hasTakenCourse(phd003, csc561, s001, 3, 4.0).

% Spring 2025 (9 credits)
% Research credits (S/U)
hasTakenCourse(phd003, csc893, s001, 9, 3.0).

% Exam outcomes
phdWrittenExamTaken(phd003, fall, 2023, 3.0).
phdOralExamTaken(phd003, spring, 2024, 3.0).

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