:- consult('swi-prolog.pl').
:- consult('mscCanGraduate.pl').
:- consult('mscTerminated.pl').
:- consult('whyCanOrCannotTake.pl').
:- consult('mscRecommend.pl').

registrationSemester(s001, msc, fall, 2023, yes).
registrationSemester(s001, msc, spring, 2024, no).
registrationSemester(s001, msc, fall, 2024, no).
registrationSemester(s001, msc, spring, 2025, no).

hasTakenCourse(s001, csc600, s001, 1, 3.0).
hasTakenCourse(s001, csc505, s001, 3, 4.33).
hasTakenCourse(s001, csc501, s001, 3, 4.0).
hasTakenCourse(s001, csc540, s001, 3, 4.0).
hasTakenCourse(s001, csc520, s001, 3, 4.0).
hasTakenCourse(s001, csc561, s001, 3, 4.0).
hasTakenCourse(s001, csc565, s001, 3, 4.0).
hasTakenCourse(s001, csc574, s001, 3, 4.0).
hasTakenCourse(s001, csc514, s001, 3, 4.0).
hasTakenCourse(s001, csc579, s001, 3, 4.0).
hasTakenCourse(s001, csc580, s001, 3, 4.0).
hasTakenCourse(s001, csc707, s001, 3, 4.0).
hasTakenCourse(s001, csc591, s001, 3, 4.0).

graduateAdvisor(s001, f001, msc).