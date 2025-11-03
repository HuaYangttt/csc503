:- consult('currentCourses.pl').
:- consult('mainUtils.pl').
:- consult('mscCanGraduate.pl').
:- consult('phdCanGraduate.pl').
:- consult('mscTerminated.pl').
:- consult('phdTerminated.pl').
:- consult('whyCanOrCannotTake.pl').
:- consult('mscRecommend.pl').
:- consult('phdRecommend.pl').


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