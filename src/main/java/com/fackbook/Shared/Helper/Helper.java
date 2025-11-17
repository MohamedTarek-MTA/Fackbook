package com.fackbook.Shared.Helper;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.Period;

@Service
@RequiredArgsConstructor
public class Helper {
    public static Pageable pageHandler(int page,int size , String sortBy , String direction){
        Sort sort = direction.equalsIgnoreCase("desc")
                ? Sort.by(sortBy).descending() : Sort.by(sortBy).ascending();
        return PageRequest.of(page,size,sort);
    }
    public static String getAge(LocalDate birthDate) {
        if (birthDate == null) {
            return "Unknown";
        }

        LocalDate now = LocalDate.now();
        Period period = Period.between(birthDate, now);

        if (period.getYears() > 0) {
            return period.getYears() + (period.getYears() == 1 ? " Year" : " Years");
        } else if (period.getMonths() > 0) {
            return period.getMonths() + (period.getMonths() == 1 ? " Month" : " Months");
        } else {
            return period.getDays() + (period.getDays() == 1 ? " Day" : " Days");
        }
    }


}
