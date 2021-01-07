fn main() {
    let ints = [0u16, 1, 7, 0x7F, 0x80, 0xFE, 0xFF];
    for (_ii,i) in ints.iter().enumerate() {
        for (_jj,j) in ints.iter().enumerate() {

            let (i,j) = (*i,*j);

            let neg_j = (j ^ 0xFFu16) + 1;
            let sum = i + neg_j;

            let unsigned_lt = i < j;
            let signed_lt = (i as i8) < (j as i8);

            println!("i=0x{:03x}=0n{}u8=0n{}i8 j=0x{:03x}=0n{}u8=0n{}i8 neg_j={:03x}=[C={},N={}] sum={:03x}=[C={},N={}]  i unsigned_< j={}  i signed_< j={}", 
                i, i as u8, i as i8,
                j, j as u8, j as i8,
                neg_j, (neg_j >> 8) & 0x1, (neg_j >> 7) & 0x1,
                sum, (sum >> 8) & 0x1, (sum >> 7) & 0x1,
                if unsigned_lt { 1 } else { 0 },
                if signed_lt { 1 } else { 0 }
                );
            assert_eq!(unsigned_lt as u16, 1 - ((sum >> 8) & 0x1));
            //assert_eq!(signed_lt as u16, (sum >> 7) & 0x1);
        }
    }
}
