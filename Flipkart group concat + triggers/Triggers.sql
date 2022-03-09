-- Triggers
/*DELIMITER $$ 
CREATE TRIGGER trigger_name [BEFORE|AFTER] [INSERT|UPDATE|DELETE] 
ON table_name FOR EACH ROW  [FOLLOWS | PRECEDES]  existing_trigger_name 
BEGIN 
   statement ;
END $$ 
DELIMITER ;
*/
Use april;
Select * from tbl;  /*tbl is the original table containing data*/

Create table tbl_back_update  like tbl ; /*Creating another table with the same structure as tbl for data backup*/

Delimiter  //
Drop trigger if exists before_tbl_update  //
Create Trigger before_tbl_update  Before  Update
On  tbl  for each row 
Begin
Insert into tbl_back_update  values (old.aut_id,old.name,old.country,old.home_city) ;
End  //
Delimiter  ;

Update tbl set name='Julie Morgan' where aut_id='Aut005' ;
Select * from tbl_back_update ; /*table to store the backup of old data before update*/
Select * from tbl;


Select * from tbl_back_update ;
Select * from tbl;

Create table tbl_new  like tbl ;

Delimiter  //
Drop Trigger if exists  after_tbl_insert  //
Create Trigger after_tbl_insert  After  Insert
on tbl  for each row 
Begin
Insert into tbl_new values (new.aut_id,new.name,new.country,new.home_city) ;
End  //
Delimiter ;

Insert into tbl values ('AUT022','Alex','Australia','Queensland');
Select * from tbl_new ;
Select * from tbl;
