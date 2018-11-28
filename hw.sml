(* Program Language Homework. Week 2. *)
(* Creator: hanrai *)

(* Helper Functions *)
(* Summary:  In order to make my code easier to read, I introduce these three helper functions: getYear, getMonth, getDay. Compare to (#1 date) for year part, getYear(date) will be easier to understand.*)

fun getYear (date : int * int * int) = (#1 date);
fun getMonth (date : int * int * int) = (#2 date);
fun getDay (date : int * int * int) = (#3 date);
val daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
val daysInMonthLeapYear = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

(* Function: is_older = fn : (int * int * int) * (int * int * int) -> bool
   	     Take two date then compare them to make sure if the first
   	     date is the older one. 

   Return:   true, Only if the firstDate is a date that comes before
   	     the secondDate.
	     false, If the firstDate that comes late the secondDate
	     or they're the same.

   Design:   To compare two dates, normally we following this rule:
   	     First, compare year part of two dates, then we have 
	     three situation. 
	     * If year part of firstDate larger than secondDate's, 
	       return false;
	     * If year part of firstDate smaller than secondDate's,
	       return true;
	     * If year part of two days are the same, test month part
	       in the same way. Then, maybe the day part.

	     But if I write code like that, I will got many
	     if-then-else clauses or logical operators stick together. 
	     The big problem is that it is not clear to read in this
	     fesion, and it's error-prone. Imagine that, can you
	     figure out the relationship with those test clauses in
	     one second?

	     So I found a new way to compare two dates. Here's my design:
	     First write a new local helper function to reformat two
	     dates with this format: yyyymmdd. 
	     It is an int number. And it is very easy to create. See,
	     month and date part need four digits so I need to move
	     year part four digits left: year * 10000.
	     Now you got year0000, for example year 2018 looks like 
	     20180000 now. 
	     Then move month part by multiply month by 100 then plus
	     the result by the formated year part.
	     Finally add the day part. Now we have a yyyymmdd data.

	     Then compare two formated dates, return true if second
	     date larger then the first one, else return false. That's
	     my way. Clear engouh right?

	     Why will this working? Because year part have the most
	     weight then month part, and month part have more weight
	     than day part. So if year part was not same, whatever
	     other part is, you will get the right answer. You can try
	     by your self.

   Disadvantage:
	     Can't deal with negative very well. And the combination
	     of date must be less than the max int limit. So it can
	     manage less data than other method.
*)

fun is_older (firstDate : int * int * int,
	      secondDate: int * int * int) =
    let fun format (date : int * int * int) =
	    getYear(date) * 10000 + getMonth(date) * 100 + getDay(date);
    in
	format(firstDate) < format(secondDate)
    end;
		  

(* Function number_in_month = fn : (int * int * int) list * int -> int
Function number_in_month that takes a list of dates and a month (i.e., an
int) and returns how many dates in the list are in the given month. *)

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates then 0 else
	if getMonth(hd dates) = month then
	    number_in_month(tl dates, month) + 1
	else
	    number_in_month(tl dates, month);


(* Function number_in_months = fn : (int * int * int) list * int list -> int 
Function number_in_months that takes a list of dates and a list of months (i.e., an
int list) and returns the number of dates in the list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint:  Use your answer to the previous problem.*)

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months then 0 else
    number_in_month(dates, hd months) + number_in_months(dates, tl months);


(* Function dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list
Function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a list holding the dates from the argument list of dates that are in the month.  The returned list should contain dates in the order they were originally given. *)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates then [] else
    if getMonth(hd dates) = month then
	(hd dates)::dates_in_month(tl dates, month)
    else
	dates_in_month(tl dates, month);


(* Function dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list
Function dates_in_months that takes a list of dates and a list of months (i.e., an int list) and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint:  Use your answer to the previous problem and SML¡¯s list-append operator (@). *)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months then [] else
    dates_in_month(dates, hd months) @ dates_in_months(dates, tl months);


(* Function get_nth = fn : string list * int -> string
Function get_nth that takes a list of strings and an int n and returns the nth element of the list where the head of the list is 1st .  Do not worry about the case where the list has too few elements: your function may apply hd or tl to the empty list in this case, which is okay. *)

fun get_nth (lst, nth : int) =
    if nth = 1 then hd lst else get_nth (tl lst, nth - 1);


(* Function date_to_string = fn : int * int * int -> string
Function date_to_string that takes a date and returns a string of the form January 20, 2013(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem.  For consistency, put a comma following the day and use capitalized English month names:  January, February, March, April,May, June, July, August, September, October, November, December. *)

fun date_to_string (date : (int * int * int)) =
    let val monthNames = ["January", "February", "March", "April", "May",
			  "June", "July", "August", "September",
			  "October", "November", "December"];
    in
	get_nth(monthNames, getMonth(date)) ^ " " ^
	Int.toString(getDay(date)) ^ ", " ^
	Int.toString(getYear(date))
    end;


(* Function number_before_reaching_sum = fn : int * int list -> int
Function number_before_reaching_sum that takes an int called sum, which you can assume is positive, and an int list, which you can assume contains all positive numbers, and returns an int. You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more.  Assume the entire list sums to more than the passed in value; it is okay for an exception to occur if this is not the case. *)

fun number_before_reaching_sum (sum : int, numbers : int list) =
    if hd numbers >= sum then 0 else
    number_before_reaching_sum(sum - (hd numbers), tl numbers) + 1;


(* Function what_month = fn : int -> int
Function what_month that takes a day of year (i.e., an int between 1 and 365) and returns what month that day is in (1 for January, 2 for February, etc.).  Use a list holding 12 integers and your
answer to the previous problem. *)

fun what_month (day : int) =
	number_before_reaching_sum(day, daysInMonth) + 1
	

(* Function month_range = fn : int * int -> int list
Function month_range that takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2.  Note the result will have length day2 - day1 + 1 or length 0 if day1>day2. *)

fun month_range (day1 : int, day2 : int) =
    if day1 > day2 then [] else
    what_month(day1)::month_range(day1 + 1, day2);


(* Function oldest = fn : (int * int * int) list -> (int * int * int) option
Function oldest that  takes  a  list  of  dates  and  evaluates  to  an (int*int*int) option.   It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
		
fun oldest (dates : (int * int * int) list) =
    if null dates then NONE else
    let
	fun compare (days : (int * int * int) list) =
	    if null (tl days) then (hd days) else
	    let
		val result = compare(tl days);
	    in
		if is_older(hd days, result) then hd days else result
	    end;
    in
	SOME (compare dates)
    end;


(* Functions number_in_months_challenge and dates_in_months_challenge that are like your solutions to problems 3 and 5 except having a month in the second argument multiple times has no more effect than having it once.  (Hint:  Remove duplicates, then use previous work.) *)

(* Helper function: distinct
Remove duplication from list *)

fun distinct (ints : int list) =
    let
	fun distinctOne (n : int, intList : int list) =
	    if null intList then n::[] else
	    if n = hd intList then distinctOne(n, tl intList) else
	    hd intList::distinctOne(n, tl intList);
	fun distinctAll (intList : int list, distList : int list) =
	    if null (tl intList) then distinctOne(hd intList, distList) else
	    distinctAll(tl intList, distinctOne(hd intList, distList));
    in
	distinctAll(ints, ints)
    end;
	    
fun number_in_months_challenge (dates : (int * int * int) list, months: int list) =
    number_in_months(dates, distinct(months));

fun dates_in_months_challenge (dates : (int * int * int) list, months: int list) =
    dates_in_months(dates, distinct(months));


(* Function reasonable_date that  takes  a  date  and  determines  if  it describes a real date in the common era.  A ¡°real date¡± has a positive year (year 0 did not exist), a month between 1 and 12, and a day appropriate for the month.  Solutions should properly handle leap years.  Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100. (Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.) *)

fun reasonable_date (date : (int * int * int)) =
    getYear(date) > 0 andalso
    getMonth(date) >= 1 andalso
    getMonth(date) <= 12 andalso
    getDay(date) >= 1 andalso
    let
	val year = getYear(date);
	val isLeapYear = year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0);
	val days = if isLeapYear then daysInMonthLeapYear else daysInMonth;
	val maxDay = get_nth(days, getMonth(date));
    in
	getDay(date) <= maxDay
    end;
