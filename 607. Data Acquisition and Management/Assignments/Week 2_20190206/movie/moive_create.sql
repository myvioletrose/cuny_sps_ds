# --create a schema 
drop schema if exists movie;
create schema movie;
use movie; 

# --create a movie table 
drop table if exists movie;
create table movie (
movie varchar(100) not null,
friend varchar(100) not null,
rating integer
)
;

# --populate the movie table with data (6 movies x 5 kids)
insert into  movie (movie, friend, rating)
values 
('Avengers: Infinity War', 'Jayden', 4),
('Black Panther', 'Jayden', 2),
('Thor', 'Jayden', 4),
('Captain America: Civil War', 'Jayden', 4),
('Guardians of the Galaxy', 'Jayden', 2),
('Ant-Man', 'Jayden', 5),
('Avengers: Infinity War', 'Ethan', 4),
('Black Panther', 'Ethan', 3),
('Thor', 'Ethan', 5),
('Captain America: Civil War', 'Ethan', 3),
('Guardians of the Galaxy', 'Ethan', 4),
('Ant-Man', 'Ethan', 1),
('Avengers: Infinity War', 'Max', 4),
('Black Panther', 'Max', 4),
('Thor', 'Max', 4),
('Captain America: Civil War', 'Max', 2),
('Guardians of the Galaxy', 'Max', 3),
('Ant-Man', 'Max', 1),
('Avengers: Infinity War', 'Jason', 4),
('Black Panther', 'Jason', 5),
('Thor', 'Jason', 5),
('Captain America: Civil War', 'Jason', 2),
('Guardians of the Galaxy', 'Jason', 2),
('Ant-Man', 'Jason', 5),
('Avengers: Infinity War', 'Eric', 2),
('Black Panther', 'Eric', 2),
('Thor', 'Eric', 5),
('Captain America: Civil War', 'Eric', 4),
('Guardians of the Galaxy', 'Eric', 5),
('Ant-Man', 'Eric', 3)
;
