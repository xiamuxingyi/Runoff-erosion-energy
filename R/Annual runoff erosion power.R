#' 计算年径流侵蚀功率
#'
#' 这个函数计算给定流域的年径流侵蚀功率。
#' 李占斌，鲁克新，李鹏，等.基于径流侵蚀功率的流域次暴雨产沙模型研究[C].第六届全国泥沙基本理论研究学术讨论会论文集.郑州:黄河水利出版社，2005：54-59.
#' 贾路，李占斌，于坤霞，等.基于径流侵蚀功率的长江典型流域能沙关系模型及改进[J].农业工程学报,2024,40(05):128-140.
#' 于坤霞，李天毅，贾路，等.黄土高原径流侵蚀功率输沙模型的改进[J].农业工程学报,2024,40(10):107-116.
#' @param data0 数据框，第一列为年份，第二列为月份，第三列及其往后为月流量值，m3/s。
#' @param Area 数据框，第一列为流域编号，第二列为流域控制水文站名称，第三列为流域面积，km2。
#' @return 返回一个矩阵，第一列为年份，第二列及其往后为年径流侵蚀功率，m4/(s-1·km-2)。
#' @export
#' @examples
#' data <- read.csv('./逐月平均流量.csv', sep =",", fileEncoding = "GB18030")
#' Area <- read.csv('./Area.csv', sep =",", T, fileEncoding = "GB18030")
#' E <- calculate_erosion_power(data, Area)
calculate_erosion_power <- function(data0, Area) {
  data0 <- as.matrix(data0)
  nl <- nrow(data0)
  m <- ncol(data0) - 2
  start <- min(data0[,1])
  end <- max(data0[,1])
  t <- seq(start, end, 1)
  tl <- length(t)
  E <- matrix(NA, tl, m + 1)
  E[,1] <- t
  colnames(E) <- colnames(data0)[-1]
  for (ij in 1:m) {
    data <- cbind(data0[,c(1:2, 2 + ij)])
    colnames(data) <- c("Year", "Month", "R")
    yue0 <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    yue1 <- c(31,29,31,30,31,30,31,31,30,31,30,31)
    for (j in 1:tl) {
      bn <- which(data[,1] == t[j])
      dat <- as.numeric(data[bn, 3]) 

      if ((t[j] / 4) == ceiling(t[j] / 4)) {
        H <- sum(dat * yue1 * 24 * 3600) / (as.numeric(Area[ij, 3]) * 1000000)  
      } else {
        H <- sum(dat * yue0 * 24 * 3600) / (as.numeric(Area[ij, 3]) * 1000000)  
      }

      E[j, 1 + ij] <- max(dat) / (as.numeric(Area[ij, 3])) * H  
      print(c(ij, t[j]))  
    }
  }
  return(E)
}

