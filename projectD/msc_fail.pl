:- consult('swi-prolog.pl').
:- consult('mscCanGraduate.pl').
:- consult('mscTerminated.pl').
:- consult('whyCanOrCannotTake.pl').
:- consult('mscRecommend.pl').

registrationSemester(s002, msc, fall, 2023, yes).
registrationSemester(s002, msc, spring, 2024, no).
registrationSemester(s002, msc, fall, 2024, no).
registrationSemester(s002, msc, spring, 2025, no).
registrationSemester(s002, msc, fall, 2025, no).

hasTakenCourse(s002, csc600, s001, 1, 3.0).
hasTakenCourse(s002, csc503, s001, 3, 3.0).
hasTakenCourse(s002, csc501, s001, 3, 3.0).
hasTakenCourse(s002, csc565, s001, 3, 3.0).
hasTakenCourse(s002, csc579, s001, 3, 3.0).
hasTakenCourse(s002, csc630, s001, 3, 3.0).
hasTakenCourse(s002, csc580, s001, 3, 3.0).