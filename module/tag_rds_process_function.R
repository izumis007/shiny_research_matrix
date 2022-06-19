#process tag and rds file 

#tgtdir <- r"(C:\Users\ma060\Desktop\test)"
tag_rds_generate_function <- function(tgtdir){
  #prepare variables-----------------
  pdfs   <- list.files(tgtdir,pattern = "pdf$") 

  message("No research_matrix_data.rds found. Create!")
  
  template <- read_csv("matrix_template.csv",col_types = cols(.default = "c"),na = character())
  
  basic_template <- tibble(pdfname = pdfs, x = list(template)) |> 
    unnest(c(x))
  
  write_rds(basic_template, str_glue("{tgtdir}/research_matrix_data.rds")) 
  
  #update tag.xlsx
  rdsdat <- basic_template
  
  pdat <- rdsdat |> select(pid,pname,cid,cname) |> filter(cid=="00") |> distinct() |> select(pid,pname)
  cdat <- rdsdat |> select(pid,pname,cid,cname) |> filter(cid!="00") |> distinct() |> 
    group_by(pid,pname) |> nest()
  
  tagdat <- pdat |> 
    left_join(cdat, by=c("pid","pname")) |> 
    mutate(nr = map_int(data,~{
      if(is.null(.)) 0L else nrow(.)
    }))
  maxtags <- max(tagdat$nr)
  
  tagdat2 <- map_dfc(1:nrow(tagdat),~{
    adat <- tagdat$data[[.]]
    if(is.null(adat)){
      vec <- c(rep("",maxtags))
    }else{
      vec <- c(tagdat$data[[.]]$cname)
    }
    
    ret <- tibble(a = vec) |> setNames(tagdat$pname[.])
    return(ret)
  })
  
  openxlsx::write.xlsx(tagdat2,str_glue("{tgtdir}/tag.xlsx"))
}


tag_rds_process_function <- function(tgtdir){
  
  #prepare variables-----------------
  pdfs   <- list.files(tgtdir,pattern = "pdf$") 
  tgtrds <- list.files(tgtdir,"research_matrix_data.rds")
  
  #update rds depend on tag.xlsx
  
  #read rds file and tag file
  rdsdat <- read_rds(file.path(tgtdir,tgtrds))
  tagdat <- readxl::read_excel(file.path(tgtdir,"tag.xlsx"))
  
  #make tagdat to longer format
  tagdat2 <- map_dfr(1:ncol(tagdat),~{
    
    num <- .
    acol <- tagdat[,num]
    cnamevec <- acol[[1]][!is.na(acol[[1]])]
    if(length(cnamevec)==0){
      cnamevec <- ".default" 
    }else{
      cnamevec <-  c(".default", cnamevec)
    }
    cidvec <- formatC(0:(length(cnamevec)-1),width=2,flag="0")
    
    tibble(
      pid     = formatC(num,width=3,flag="0"),
      pname   = colnames(acol),
      cid     = cidvec,
      cname   = cnamevec
    )
  })
  
  #nestrds dat
  rdsdat2 <- rdsdat |> 
    group_by(pid,pname,cid,cname) |> 
    nest()
  
  #   rdsdat    tagdat
  #   A         A      :(1) check order
  #             B      :(2)add B to rdsdat
  #   C                :(3)change status as removed
  
  joined <- rdsdat2 |> 
    full_join(tagdat2,
              by=c("pname","cname"),
              suffix=c("_1","_2"),
              keep = TRUE)
  
  #(1) check order
  d1 <- joined |> 
    filter(!is.na(pid_1) & !is.na(pid_2)) |> 
    mutate(pid = pid_2, pname=pname_2, cid = cid_2, cname=cname_2) |> 
    select(pid,pname,cid,cname,data)
  
  #(2)
  d2 <- joined |> 
    filter(is.na(pid_1) & !is.na(pid_2)) |> 
    mutate(pid = pid_2, pname=pname_2, cid = cid_2, cname=cname_2) |> 
    select(pid,pname,cid,cname,data)
  
  #(3)
  d3 <- joined |> 
    filter(!is.na(pid_1) & is.na(pid_2)) |> 
    mutate(pid = pid_1, pname=pname_1, cid = cid_1, cname=cname_1) |> 
    select(pid,pname,cid,cname,data) |> 
    mutate(pid = "removed", cid = "removed")
  
  #save this data as new rds data
  updated_rdsdata <- bind_rows(d1,d2,d3) |> 
    arrange(pid,cid) |> 
    mutate(data = map(data, ~{
      adata <- .
      if(is.null(adata)){
        res <- tibble(pdfname = pdfs, val ="")
      }else{
        #check if new pdf file added from past
        is_exist <- pdfs %in% adata$pdfname
        #add if not exist
        pdfs_not_exist <- pdfs[!is_exist]
        res <- adata |> 
          add_row( tibble(pdfname = pdfs_not_exist, val="") )
      }
      
      return(res)
    })) 
  
  removed_tag <- updated_rdsdata |> 
    filter(pid == "removed" | cid == "cid") |> 
    select(!data) |> 
    select(pname, cname)
    
  updated_rdsdata <- updated_rdsdata |> 
    unnest(data)
  
  write_rds(updated_rdsdata, file.path(tgtdir,tgtrds))
  #make removed tag xlsx
  openxlsx::write.xlsx(removed_tag,file.path(tgtdir,"removedtag.xlsx"),overwrite = TRUE)
  
}