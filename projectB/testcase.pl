% =========================
% PhD test case 5: s009 (Fails only 700-level requirement)
% =========================

registrationSemester(s009, phd, fall, 2019, yes).
registrationSemester(s009, phd, spring, 2020, no).
registrationSemester(s009, phd, fall, 2020, no).
registrationSemester(s009, phd, spring, 2021, no).
registrationSemester(s009, phd, fall, 2021, no).
registrationSemester(s009, phd, spring, 2022, no).
registrationSemester(s009, phd, fall, 2022, no).
registrationSemester(s009, phd, spring, 2023, no).
registrationSemester(s009, phd, fall, 2023, no).
registrationSemester(s009, phd, spring, 2024, no).
registrationSemester(s009, phd, fall, 2024, no).
registrationSemester(s009, phd, spring, 2025, no).

hasTakenCourse(s009, csc600, s001, 1, 4.000).
hasTakenCourse(s009, csc503, s001, 3, 3.700).
hasTakenCourse(s009, csc505, s001, 3, 3.700).
hasTakenCourse(s009, csc510, s001, 3, 3.700).
hasTakenCourse(s009, csc501, s001, 3, 3.700).
hasTakenCourse(s009, csc540, s001, 3, 3.700).
hasTakenCourse(s009, csc714, s001, 3, 3.700).  % Only 1 700-level
hasTakenCourse(s009, csc520, s001, 3, 4.000).
hasTakenCourse(s009, csc570, s001, 3, 4.000).
hasTakenCourse(s009, csc574, s001, 3, 4.000).
hasTakenCourse(s009, csc565, s001, 3, 4.000).
hasTakenCourse(s009, csc830, s001, 3, 3.333).
hasTakenCourse(s009, csc830, s001, 3, 3.333).
hasTakenCourse(s009, csc830, s001, 3, 3.333).
hasTakenCourse(s009, csc890, s001, 6, 3.333).
hasTakenCourse(s009, csc893, s001, 9, 3.333).
hasTakenCourse(s009, csc893, s001, 9, 3.333).
hasTakenCourse(s009, csc895, s001, 9, 3.333).

phdWrittenExamTaken(s009, fall, 2020, pass).
phdOralExamTaken(s009, spring, 2021, pass).
phdDefenseTaken(s009, spring, 2025, pass).

graduateAdvisor(s009, advisor2, phd).
advisoryCommitteeMember(s009, advisor2).
advisoryCommitteeMember(s009, member2).
advisoryCommitteeMember(s009, member3).
advisoryCommitteeMember(s009, member4).
planOfGraduateWorkApproved(s009).

% =========================
% PhD test case 6: s010 (Early student, multiple deficiencies)
% =========================

registrationSemester(s010, phd, fall, 2023, yes).
registrationSemester(s010, phd, spring, 2024, no).
registrationSemester(s010, phd, fall, 2024, no).
registrationSemester(s010, phd, spring, 2025, no).

hasTakenCourse(s010, csc600, s001, 1, 4.000).
hasTakenCourse(s010, csc503, s001, 3, 3.700).  % Only 1 theory
hasTakenCourse(s010, csc501, s001, 3, 3.700).  % Only 1 systems
hasTakenCourse(s010, csc714, s001, 3, 3.700).  % Only 1 700-level
hasTakenCourse(s010, csc520, s001, 3, 4.000).
hasTakenCourse(s010, csc830, s001, 3, 3.333).
hasTakenCourse(s010, csc890, s001, 3, 3.333).  % Only 3 units (needs 6)

phdWrittenExamTaken(s010, fall, 2024, pass).
% No oral or defense yet

graduateAdvisor(s010, advisor2, phd).
advisoryCommitteeMember(s010, advisor2).
advisoryCommitteeMember(s010, member2).
advisoryCommitteeMember(s010, member3).
advisoryCommitteeMember(s010, member4).
planOfGraduateWorkApproved(s010).