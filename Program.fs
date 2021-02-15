//
// F# program to analyze payroll data.
//
// name: Dmytro Kuzmyk   
// netid: dkuzmy3
// U. of Illinois, Chicago
// CS 341, Spring 2020
// Project #02
//

module Project02


//
// doubleOrNothing
//
// Given a string containing a double numeric value
// or being an empty string
// returns the double equivalent, with the empty string
// treated as the value 0.0
//
let doubleOrNothing s = 
    match s with
    | "" -> 0.0
    | x -> double x

//
// ParseCSVLine and ParseCSVDatabase
//
// Given a sequence of strings representing payroll data, 
// parses the strings and returns a list of tuples.  Each
// sub-list denotes one employee.  Example:
//
//[ ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0); ... ]
//
// The values are first name, last name, occupation,
// department, salary type, hours per week, annual salary,
// and hourly wage. 
// Depending on the salary type, 
// either hours per week and hourly wage 
// or annual salary will be filled in with 0.0,
// since the field is empty in the csv
//
// First Name, Last Name, Occupation, Dept Name, Fulltime or Part time, Typical Hours, Annual Salary, Hourly Rate
let ParseCSVLine (line:string) = 
    let tokens = line.Split(',')
    let listOfValues = Array.toList tokens
    //match listOfValues with  // alternative implmentation to remove warnings
    //| fName::lName::occupation::department::salaryType
    //    ::hoursPerWeek::annualSalary::hourlyWage::[] -> (fName,lName,occupation,department,salaryType,
    //                                                      (doubleOrNothing hoursPerWeek),
    //                                                      (doubleOrNothing annualSalary),
    //                                                      (doubleOrNothing hourlyWage)
    //                                                    )
    //| _ -> failwith "Insufficient values in line of csv file"

    let fName::lName::Occupation::Department::SalaryType
        ::HoursPerWeek::AnnualSalary::HourlyWage::[] = listOfValues
    (fName,lName,Occupation,Department,SalaryType,
      (doubleOrNothing HoursPerWeek),
      (doubleOrNothing AnnualSalary),
      (doubleOrNothing HourlyWage)
    )

let rec ParseCSVDatabase lines = 
    let employees = Seq.map ParseCSVLine lines
    //printfn "%A" employees
    Seq.toList employees


// Returns whether the employee is salaried
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// true
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// false 
//
let isSalary employee =
    let (fName,lName,Occupation,Department,SalaryType,
         HoursPerWeek, AnnualSalary, HourlyWage
        ) = employee
    //SalaryType = "Salary"
    match AnnualSalary with
    |a -> if a > 0.0 then true else false

// Returns whether the employee is paid hourly, not salaried
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// false
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// true 
//
let isHourly employee =
    if isSalary employee = false then true else false

// Returns the full name of the employee
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// "JESSE A ACOSTA"
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// "JOHN R KEATING"
//
let getName employee =
    let (fName,lName,Occupation,Department,SalaryType,
         HoursPerWeek, AnnualSalary, HourlyWage
        ) = employee
    fName + " " + lName

// Returns name of the department the employee belongs to
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// "POLICE"
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// "GENERAL SERVICES" 
//        
let getDepartment employee =
    let (fName,lName,Occupation,Department,SalaryType,
         HoursPerWeek, AnnualSalary, HourlyWage
        ) = employee
    Department

//
//The function calcSalary calculates the annual salary, 
// either by directly returning the recorded annual salary 
// or calculating it by finding the average weekly salary 
// by multiplying the hours per week by hourly wage, 
// and then multiplying that by 52 to get the average 
// annual salary for that hourly worker.    
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// 93354.0
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// 52.0*40.0*52.35 = 108888.0 
//
let calcSalary employee =
    let (fName,lName,Occupation,Department,SalaryType,
         HoursPerWeek, AnnualSalary, HourlyWage
        ) = employee
    if AnnualSalary > 0.00 then AnnualSalary else (HourlyWage*HoursPerWeek)*52.00
        
// The function getNumberOfEmployees returns the number of employees in the data set.
let rec getNumberOfEmployees allData =
    match allData with
    | [] -> 0
    | a::rest -> 1+getNumberOfEmployees(rest)

// The function getNumberOfSalariedEmployees returns the number of employees who have an annual salary in the data set.
let rec getNumberOfSalariedEmployees allData =
   match allData with
   | [] -> 0
   | a::rest -> if isSalary a then 1+getNumberOfSalariedEmployees rest else getNumberOfSalariedEmployees rest

// The function getNumberOfSalariedEmployees returns the number of employees who are paid hourly in the data set.
let rec getNumberOfHourlyEmployees allData =
    match allData with
    | [] -> 0
    | a::rest -> if isHourly a then 1+getNumberOfHourlyEmployees rest else getNumberOfHourlyEmployees rest

// The function findHighestPaidEmployee returns the name and salary
// of the highest paid employee.
// Use the computed salary for hourly employees.
let rec findHighestPaidEmployee allData =
    let rec loop allData tot =
        match allData, tot with
        | [],_ -> tot
        | a::rest, (x,y) -> if calcSalary a > y then loop rest (getName a,calcSalary a) else loop rest tot
    loop allData ("",0.0)

// The function findHighestPaidEmployee returns the salary
// of the highest paid employee within a specific department.
// Use the computed salary for hourly employees.
let rec findHighestPaidEmployeeInDept allData deptName =
    let rec loop allData deptName tot =
        match allData with
        | [] -> tot
        | a::rest -> if (getDepartment a = deptName) && (calcSalary a > tot) then loop rest deptName (calcSalary a) else loop rest deptName tot
    loop allData deptName (0.0)

// The function getAverageSalary calculates
// the average of the computed salary field.
let rec getAverageSalary allData =
    (List.sum (List.map (calcSalary) allData)) / float(getNumberOfEmployees allData)

// The function getAverageSalary calculates 
// the average of the computed salary field for a specific department.
let rec getAverageSalaryInDept allData deptName =
    getAverageSalary(List.filter (fun x -> getDepartment x = deptName) allData)
    

// Searches through the data set to generate the list of all unique department names.
let getUniqueDepartmentNames allData =
    allData
    |> List.map getDepartment
    |> List.distinct
    |> List.sort
    //without piping
    //let allDepts = List.map getDepartment allData
    //let uniqueDepts = List.distinct allDepts
    //uniqueDepts


// The function howManyEmployeesInEachDepartment computes the number of employees in every department.  
// This function should return a list of tuples, pairs between the department name and number of employees. 
let rec howManyEmployeesInEachDepartment allData deptNames =
    //if allData = [] then [("No Department", 0)] else 
    let rec loop ad dn ret arr =
        match ad, dn with
        | _ , [] -> arr
        | x::y, a::b -> if getDepartment x = a then loop y (a::b) (1+ret) arr else loop y (a::b) (ret) arr
        | [], a::b -> loop allData b 0 (ret::arr)
    
    List.zip deptNames (List.rev (loop allData deptNames 0 []))

    
// The function findTotalSalaryByDepartment computes the overall annual salary budget for every department. 
// The calculated salary should include the average annual salary for hourly employees.
// This function should return a list of tuples, pairs between the department name and total annual salary. 
let rec findTotalSalaryByDepartment allData deptNames =
    //[("No Department",0.0)]
    let rec loop ad dn ret arr =
        match ad, dn with
        | _ , [] -> arr
        | x::y, a::b -> if getDepartment x = a then loop y (a::b) ((calcSalary x)+ret) arr else loop y (a::b) (ret) arr
        | [], a::b -> loop allData b 0.0 (ret::arr)
    
    List.zip deptNames (List.rev (loop allData deptNames 0.0 []))
        
// The function findHighestPaidDeptOverall returns the name and total annual salary
// of the department with the largest overall annual salary paid to employees in that department. 
// The calculated salary should include the average annual salary for hourly employees.
// This function should return a single tuple, containing the department name and total annual salary. 
let rec findHighestPaidDeptOverall allData deptNames =
    //("No Department",0.0)
    let rec loop ad dn ret arr =
        match ad, dn with
        | _ , [] -> arr
        | x::y, a::b -> if getDepartment x = a && calcSalary x > ret then loop y (a::b) (calcSalary x) arr else loop y (a::b) (ret) arr
        | [], a::b -> loop allData b 0.0 (ret::arr)
    
    List.zip deptNames (List.rev (loop allData deptNames 0.0 []))

// The function withinSalaryRange returns the number of employees whose calculated salary
// is greater than the lower bound and less than or equal to the upper bound.
let rec withinSalaryRange lower upper L =
    match L with
    | [] -> 0
    | x::rest -> if calcSalary x >= lower && upper >= calcSalary x then 1+(withinSalaryRange lower upper rest) else withinSalaryRange lower upper rest

//
// printOneHistogram
//
// prints one histogram value, the format is
//   label: *****value
// where the # of stars is value / amountPerStar.
//
let printOneHistogram label value amountPerStar =
  // 
  // helper function to print # of stars:
  //
  let rec printstars n = 
    match n with
    | 0 -> ()
    | 1 -> printf "*"
    | _ -> printf "*"
           printstars (n-1)
  //
  // print label, stars, and histogram value:
  //
  printf " %16s: " label    // Enough space for all the departments in the file
  printstars (value / amountPerStar)
  printfn "%A" value




[<EntryPoint>]
let main argv =
    //
    // input file name, then input employee data and build
    // a list of tuples:
    //
    printf "Enter name of the csv file containing employee data: "

    let filename = System.Console.ReadLine() //"payroll_02.csv" //
    let contents = System.IO.File.ReadLines(filename)
    let data = ParseCSVDatabase contents
    //let data = [List.head data]

    //printfn "This is the data you have loaded."
    //List.iter (printfn "%A") data

    printf "Enter name of the department to be analyzed: "

    let deptname = System.Console.ReadLine()
    let allDepts = getUniqueDepartmentNames data

    let N = getNumberOfEmployees data
    printfn ""
    printfn "# of employees: %A" N
    printfn ""

    let deptNs = howManyEmployeesInEachDepartment data allDepts
    //printfn "%A" deptNs

    let num = List.find (fun (x,y) -> x=deptname) deptNs
    let (depDep, numNum) = num

    printfn "# of employees in %s: %d" deptname numNum       // Figure out how to get the number for one department
    printfn ""

    //
    // % of employees salaried:
    //
    let numSalaried = getNumberOfSalariedEmployees data
    let percentSalaried = (double numSalaried) / (double N) * 100.0
    let numHourly = getNumberOfHourlyEmployees data
    let percentHourly = (double numHourly) / (double N) * 100.0

    printfn "%% of employees Salaried: %d (%.2f%%)" numSalaried percentSalaried
    printfn "%% of employees Hourly: %d (%.2f%%)" numHourly percentHourly
    printfn ""

    //
    // average salary:
    //
    let avgSalary = getAverageSalary data
    printfn "Average salary: %.2f" avgSalary
    //printfn ""

    //
    // average salary in department:
    //
    let avgSalary = getAverageSalaryInDept data deptname
    printfn "Average salary in %s: %.2f" deptname avgSalary
    printfn ""
    
    //
    // highest salary:
    //
    let (maxName,maxSalary) = findHighestPaidEmployee data
    printfn "Largest salary: %s paid %.2f annually" maxName maxSalary
    //printfn ""

    //
    // highest salary in department:
    //
    let maxSalary = findHighestPaidEmployeeInDept data deptname
    printfn "Largest salary in %s: %.2f" deptname maxSalary
    printfn ""

    printfn "** Histogram of employees by department (each star represents 5 employees):"    
    // Use printOneHistogram to build the histogram
    //printfn "" 

    //let num2 dept = List.find (fun (x,y) -> x=dept) deptNs
    //let (depDep2, numNum2) = num2

    (*for i in (getUniqueDepartmentNames data) do
        let num2 = List.find (fun (x,y) -> x=i) deptNs
        let (depDep2, numNum2) = num2
        printOneHistogram (depDep2) (numNum2) (5)*)

    List.iter (fun (x,y) -> printOneHistogram x y (5)) deptNs

    printfn "" 
    //
    // categorize salaries into 5 groups:
    //   0       < salary <= 60000
    //   60000   < salary <= 80000
    //   80000   < salary <= 100000
    //   100000  < salary <= 120000
    //   120000  < salary <= 10,000,000  // arbitrary upper bound to reuse function
    //
    
    let count60korless = List.length(List.filter (fun x -> calcSalary x <= 60000.00) data)
    let percent60korless = (float(count60korless)/float(getNumberOfEmployees data))*100.00 

    let count60kto80k = List.length(List.filter (fun x -> calcSalary x >= 60000.00 && calcSalary x <= 80000.00) data)
    let percent60kto80k = (float(count60kto80k)/float(getNumberOfEmployees data))*100.00

    let count80kto100k = List.length(List.filter (fun x -> calcSalary x >= 80000.00 && calcSalary x <= 100000.00) data)
    let percent80kto100k = (float(count80kto100k)/float(getNumberOfEmployees data))*100.00

    let count100kto120k = List.length(List.filter (fun x -> calcSalary x >= 100000.00 && calcSalary x <= 120000.00) data)
    let percent100kto120k = (float(count100kto120k)/float(getNumberOfEmployees data))*100.00

    let countgreater120k = List.length(List.filter (fun x -> calcSalary x > 120000.00) data)
    let percentgreater120k = (float(countgreater120k)/float(getNumberOfEmployees data))*100.00

    printfn "** Salary Ranges:"
    printfn " 0-60000 : %A (%.2f%%)" count60korless percent60korless
    printfn " 60000-80000 : %A (%.2f%%)" count60kto80k percent60kto80k
    printfn " 80000-100000: %A (%.2f%%)" count80kto100k percent80kto100k
    printfn " 100000-120000: %A (%.2f%%)" count100kto120k percent100kto120k
    printfn " > 120000: %A (%.2f%%)" countgreater120k percentgreater120k
    printfn ""

    printfn "** Histogram of Salary Ranges (each star represents 10 employees):"    
    let salaryGroups = [("<60000", count60korless);("60-80k", count60kto80k);("80-100k", count80kto100k);("100-120k", count100kto120k);(">120000",countgreater120k)]
    // Use printOneHistogram to build the histogram

    (*for i in salaryGroups do
        let (depDep2, numNum2) = i
        printOneHistogram (depDep2) (numNum2) (10)
    *)

    List.iter (fun (x,y) -> printOneHistogram x y (10)) salaryGroups

    0    