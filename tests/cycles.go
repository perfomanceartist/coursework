package main
import "fmt"
 
func main() {
     
    for i := 1; i < 10; i++{
        for j := 1; j < 10; j++{
            fmt.Print(i * j, "\t")
        }
		if (value < 0){
			continue        // переходим к следующей итерации
		}
		
		break
        fmt.Println()
    }

	var users = [3]string{"Tom", "Alice", "Kate"}
	for index, value := range users{
		fmt.Println(index, value)
	}

}