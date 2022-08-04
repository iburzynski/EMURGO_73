students = [
    ("Eddie", 'C'),
    ("Bjoern", 'C'),
    ("Praszad", 'B'),
    ("Thierry", 'C'),
    ("Javi", 'B'),
    ("Matthew", 'A'),
    ("Shivam", 'B'),
    ("Pradeep", 'C'),
    ("Saeid", 'C'),
    ("Manuel", 'C'),
    ("Gary", 'A'),
    ("Beulah", 'B'),
    ("Tien", 'B'),
    ("Ian", 'T')
    ]

def group_students(students):
    group_a, group_b, group_c = ([], [], [])

    for (name, group) in students:
        match group:
            case 'A':
                group_a.append(name)
            case 'B':
                group_b.append(name)
            case 'C':
                group_c.append(name)

    return (group_a, group_b, group_c)

print (group_students(students))