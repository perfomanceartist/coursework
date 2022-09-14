package main
import "fmt"
 
func main() {
		go call(x )

        go func(n int) int{
            result := 1
            for j := 1; j <= n; j++{
                result *= j
            }
            fmt.Println(n, "-", result)
			return result
        }(i)
    
}