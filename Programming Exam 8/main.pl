:- module(main, [sum_age/2, max_age_of_hobby/3, person_in_range/4]).
:- [kb].

% DO NOT CHANGE THE UPPER CONTENT, WRITE YOUR CODE AFTER THIS LINE
sum_age([], 0).
sum_age([A|B], Sum) :- person(A, Age, _), sum_age(B, Rest), Sum is Age + Rest.

max_age_of_hobby([], _, 0).
max_age_of_hobby([A|B], Hobby, MaxAge) :- person(A, Age, Hobbies), max_age_of_hobby(B, Hobby, Rest), Hobbies = Hobby, MaxAge is max(Age, Rest).
max_age_of_hobby([A|B], Hobby, MaxAge) :- person(A, Age, Hobbies), max_age_of_hobby(B, Hobby, Rest), Hobbies \= Hobby, MaxAge is Rest.

person_in_range([], _, _, []).
person_in_range([A|B], Min, Max, Result) :- person(A, Age, _), Age >= Min, Age =< Max, person_in_range(B, Min, Max, Rest), append([A], Rest, Result).
person_in_range([A|B], Min, Max, Result) :- person(A, Age, _), Age < Min, person_in_range(B, Min, Max, Rest), append([], Rest, Result).
person_in_range([A|B], Min, Max, Result) :- person(A, Age, _), Age > Max, person_in_range(B, Min, Max, Rest), append([], Rest, Result).