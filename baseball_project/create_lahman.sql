create database lahman;

use lahman;

create table if not exists master (
  masterId int(11),
  primary key (masterId),
  playerID varchar(20),
  birthYear int(4),
  birthMonth int(2),
  birthDay int(2),
  birthCountry varchar(3),
  birthState varchar(2),
  birthCity varchar(20),
  deathYear int(4),
  deathMonth int(2),
  deathDay int(2),
  deathCountry varchar(3),
  deathState varchar(2),
  deathCity varchar(20),
  nameFirst varchar(20),
  nameLast varchar(20),
  nameGiven varchar(20),
  weight int(3),
  height int(3),
  bats enum('R','L'),
  throws enum('R','L'),
  debut datetime,
  finalGame datetime,
  retroID varchar(20),
  bbrefID varchar(20)
);

load data local infile '/Users/aimeebarciauskas/Desktop/lahman-csv_2015-01-24/Master.csv'
  into table master fields terminated by ','
  lines terminated by '\n'
  (playerID,birthYear,birthMonth,birthDay,birthCountry,birthState,birthCity,deathYear,deathMonth,deathDay,deathCountry,deathState,deathCity,nameFirst,nameLast,nameGiven,weight,height,bats,throws,debut,finalGame,retroID,bbrefID);
