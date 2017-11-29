fun is_older( date1 : int*int*int, date2 : int*int*int ) =
  if (#1 date1) < (#1 date2)
  then true
  else if (#1 date1) > (#1 date2)
  then false
  else if (#2 date1) < (#2 date2)
  then true
  else if (#2 date1) > (#2 date2)
  then false
  else if (#3 date1) < (#3 date2)
  then true
  else if (#3 date1) > (#3 date2)
  then false
  else false
fun number_in_month (list1 : (int * int * int) list, month : int ) =
  if null list1
  then 0
  else
      let val head = hd list1
      in
	  if  (#2 head) = month
	  then 1 + number_in_month(tl list1, month)
	  else number_in_month(tl list1, month)
      end
	  
fun number_in_months (list1 : (int * int * int) list, months : int list) =
  if( null list1 orelse null months )
  then 0
  else
      let val month = hd months
      in
	  number_in_month(list1, month)+number_in_months(list1, tl months)
end

fun dates_in_month (list2 : (int * int * int ) list, month : int) =
  if null list2
  then []
  else
      let val head = hd list2
      in
	  if (#2 head) = month
	  then
	      head :: dates_in_month( tl list2, month)
	  else
	      dates_in_month(tl list2, month)
      end
fun dates_in_months (list2 : (int * int * int ) list, months : int list) =
  if null list2 orelse null months
  then []
  else
      let val month = hd months
      in
	  dates_in_month (list2, month) @ dates_in_months(list2, tl months)
      end

fun count_list_items(list3: string list) =
  
  if null list3
  then 0
  else
      1 + count_list_items(tl list3)
			  
fun get_nth (str_list: string list, n: int) =
  if n=1
  then hd str_list
  else
      get_nth(tl str_list, n-1)

val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	     
fun date_to_string (date: int * int * int) =
  get_nth(months, (#2 date)) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString(#1 date)

fun number_before_reaching_sum (sum: int, list1: int list) =
  if sum<=0
  then (0-1)
  else
      1 + number_before_reaching_sum (sum-(hd list1), tl list1)
      
fun what_month (day: int) =
  let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      number_before_reaching_sum(day, days)+1
  end

fun month_range (day1: int, day2: int) =
  if day1>day2
  then []
  else
  what_month(day1) :: month_range(day1+1, day2)
      
fun oldest (dates: (int * int * int) list) =
  if null dates
  then NONE
  else
      let fun pass_date (dates: (int * int * int) list, oldest: int * int * int) =
		    if null dates
		    then oldest
		    else
		    if is_older(hd dates, oldest)
			 then  pass_date(tl dates, hd dates)
	  else
	      pass_date(tl dates, oldest)
      in
	  if null (tl dates)
	  then SOME (hd dates)
	  else
	      SOME (pass_date(tl dates, hd dates))
      end
	  
			  
